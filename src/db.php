<?php
    session_start();
    header('Access-Control-Allow-Origin: *');
    ini_set('display_startup_errors', 1);
    ini_set('display_errors', 1);
    error_reporting(-1);
    $conn = new PDO('mysql:host=localhost;dbname=1998294_db', 'root', '1234');
    //$conn = new PDO('mysql:host=localhost;dbname=1183032', '1183032', 'Steinmetzstr9');
    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    if(isset($_POST['action']) && $_POST['action'] == 'load')
    {
        $fc_shapes = load_fc_shapes();
        $fc_arrows = load_fc_arrows();
        $re = array('fcShapes' => $fc_shapes, 'fcArrows' => $fc_arrows);
        echo json_encode($re, JSON_NUMERIC_CHECK);
    }
    else if(isset($_POST['action']) && $_POST['action'] == 'save')
    {
        $pap = json_decode($_POST['flowchart'], true);
        save_fc_shapes($pap['shapes']);
        save_fc_arrows($pap['arrows']);
    }


function get_user_id($name, $password)
{
    global $conn;
    $statement = $conn->prepare('
        select id
        from user
        where name = :name
          and password = :password
        limit 1');
    $statement->bindParam(':name', $name);
    $statement->bindParam(':password', $password);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    if(count($rows) === 1)
    {
        return $rows[0]['id'];
    }
    return -1;
}

function does_user_exist($name)
{
    global $conn;
    $statement = $conn->prepare('
        select id
        from user
        where name = :name
        limit 1');
    $statement->bindParam(':name', $name);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return (count($rows) >= 1);
}

function create_user($name, $password, $email)
{
    global $conn;
    if(does_user_exist($name))
    {
        return false;
    }
    $statement = $conn->prepare('
        insert into user (name, password, email) values(:name, :password, :email)');
    $statement->bindParam(':name', $name);
    $statement->bindParam(':password', $password);
    $statement->bindParam(':email', $email);
    $statement->execute();
    return true;
}


function load_fc_shapes()
{
    global $conn;
    $statement = $conn->prepare('
        select fc_shape.id as id, x, y, fc_shape_type.name as shapeType, title, text
        from fc_shape
        join book
          on fc_shape.book_id = book.id
        join fc_shape_type
          on fc_shape.fc_shape_type_id = fc_shape_type.id
        where
          book_id = (select max(book.id)
                     from book
                     where book.name = :book_name
                     and book.user_id = :user_id)');
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':book_name', $_SESSION['book_name']);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}

function load_fc_arrows()
{
    global $conn;
    $statement = $conn->prepare('
        select fc_arrow.id
             , fc_arrow.source_id
             , fc_arrow.destination_id
             , fc_arrow.source_offset_x
             , fc_arrow.source_offset_y
             , fc_arrow.destination_offset_x
             , fc_arrow.destination_offset_y
             , fc_arrow.title
        from fc_arrow
        join book
          on fc_arrow.book_id = book.id
        where
          fc_arrow.book_id = (select max(book.id)
                              from book
                              where book.name = :book_name
                              and book.user_id = :user_id)'); 
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':book_name', $_SESSION['book_name']);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}






function save_fc_shapes($fc_shapes)
{
    global $conn;


    $statement = $conn->prepare('select max(id) as max_book_id from book');
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $nextBookId = ((count($rows) === 0) ? 1 : ($rows[0]['max_book_id'] + 1));

    $statement = $conn->prepare('
        insert into book(id, user_id, name, creation_date) values(:next_book_id, :user_id, :book_name, now())');
    $statement->bindParam(':next_book_id', $nextBookId);
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':book_name', $_SESSION['book_name']);
    $statement->execute();
    if(is_null($fc_shapes))
    {
        return;
    }
    foreach($fc_shapes as $fc_shape)
    {
        $statement = $conn->prepare('
            insert into fc_shape
              (id, book_id, x, y, fc_shape_type_id, title, text)
            values (:id, :max_book_id, :x, :y, (select id from fc_shape_type where fc_shape_type.name = :type), :title, :text)');
        $statement->bindParam(':id', $fc_shape['id']);
        $statement->bindParam(':max_book_id', $nextBookId);
        $statement->bindParam(':x', $fc_shape['x']);
        $statement->bindParam(':y', $fc_shape['y']);
        $statement->bindParam(':type', $fc_shape['shapeType']);
        $statement->bindParam(':title', $fc_shape['title']);
        $statement->bindParam(':text', $fc_shape['text']);
        $result = $statement->execute();
    }
}
function save_fc_arrows($fc_arrows)
{
    global $conn;
    if(is_null($fc_arrows))
    {
        return;
    }
    foreach($fc_arrows as $fc_arrow)
    {
        $statement = $conn->prepare('
            insert into fc_arrow
              (id, source_id, destination_id, book_id, source_offset_x, source_offset_y, destination_offset_x, destination_offset_y, title)
            values (:id, :source_id, :destination_id, (select max(book.id) from book where book.name = :book_name and book.user_id = :user_id), :source_offset_x, :source_offset_y, :destination_offset_x, :destination_offset_y, :title)');
        $statement->bindParam(':id', $fc_arrow['id']);
        $statement->bindParam(':source_id', $fc_arrow['source_id']);
        $statement->bindParam(':destination_id', $fc_arrow['destination_id']);
        $statement->bindParam(':book_name', $_SESSION['book_name']);
        $statement->bindParam(':user_id', $_SESSION['user_id']);
        $statement->bindParam(':source_offset_x', $fc_arrow['source_offset_x']);
        $statement->bindParam(':source_offset_y', $fc_arrow['source_offset_y']);
        $statement->bindParam(':destination_offset_x', $fc_arrow['destination_offset_x']);
        $statement->bindParam(':destination_offset_y', $fc_arrow['destination_offset_y']);
        $statement->bindParam(':title', $fc_arrow['title']);
        $result = $statement->execute();
    }
}

function getBooks($user_id)
{
    global $conn;
    $statement = $conn->prepare('
        select book.name
        from book
        join user on book.user_id = user.id
        where user.id = :user_id
        group by book.name');
    $statement->bindParam(':user_id', $user_id);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}
function doesBookExist($user_id, $bookName)
{
    global $conn;
    $statement = $conn->prepare('
        select book.id
        from book
        join user
          on book.user_id = user.id
        where book.name = :book_name
          and user.id = :user_id
        limit 1');  
    $statement->bindParam(':book_name', $bookName);
    $statement->bindParam(':user_id', $user_id);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return count($rows) > 0;
}

function createNewBook($user_id, $bookName)
{
    if(doesBookExist($user_id, $bookName))
    {
        return false;
    }
    global $conn;
    $statement = $conn->prepare('
        insert into book(user_id, name, creation_date) values(:user_id, :book_name, now())');
    $statement->bindParam(':user_id', $user_id);
    $statement->bindParam(':book_name', $bookName);
    $statement->execute();
    $statement = $conn->prepare('
        insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(1, (select max(id) from book where name = :book_name and user_id = :user_id), 1, 100, 100, "Title", "Texte")');
    $statement->bindParam(':book_name', $bookName);
    $statement->bindParam(':user_id', $user_id);
    $statement->execute();
    return true;
}
function createNewUser($user, $password)
{
    global $conn;
    $statement = $conn->prepare('
        insert into user(name, password) values(:user, :password)');
    $statement->bindParam(':user', $user);
    $statement->bindParam(':password', $password);
    $statement->execute();
}
?>



