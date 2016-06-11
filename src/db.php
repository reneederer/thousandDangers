<?php
    ini_set('display_startup_errors', 1);
    ini_set('display_errors', 1);
    error_reporting(-1);
    $_SESSION['user_id'] = 1;
    $_SESSION['1000dangersbook_name'] = '1000 Gefahren';
    $conn = new PDO('mysql:host=localhost;dbname=1998294_db', 'root', '1234');
    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    //$conn = new PDO('mysql:host=fdb6.biz.nf;dbname=1998294_db', '1998294_db', 'Nuernberg12');
    header('Access-Control-Allow-Origin: http://localhost:8000');
    /*
    echo '
        {
            "fcShapes": [
                            { "id":"1"
                            , "x":50
                            , "y":70
                            ,  "shapeType":"Start"
                            , "title":"Starttitle"
                            , "text":"text"},
                            { "id":2
                            , "x":450
                            , "y":70
                            ,  "shapeType":"Action"
                            , "title":"Meine Action"
                            , "text":"text"}
                        ]
        }';
     //*/
    $papElements = loadPapElements();
    $papConnections = loadPapConnections();
    $re = array('fcShapes' => $papElements, 'fcArrows' => $papConnections);
    echo json_encode($re, JSON_NUMERIC_CHECK);





function loadPapElements()
{
    global $conn;
    $statement = $conn->prepare('
        select papelement.id as id, x, y, paptype.name as shapeType, title, text
        from papelement
        join 1000dangersbook
          on papelement.1000dangersbook_id = 1000dangersbook.id
        join user
           on 1000dangersbook.user_id = :user_id
        join paptype
          on papelement.paptype_id = paptype.id
        where
          1000dangersbook_id = (select max(1000dangersbook.id) from 1000dangersbook where 1000dangersbook.name = :1000dangersbook_name)');
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':1000dangersbook_name', $_SESSION['1000dangersbook_name']);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}

function loadPapConnections()
{
    global $conn;
    $statement = $conn->prepare('
        select papconnection.source_id, papconnection.destination_id, papconnection.source_offset_x, papconnection.source_offset_y, papconnection.destination_offset_x, papconnection.destination_offset_y, papconnection.title
        from papconnection
        join 1000dangersbook
          on papconnection.1000dangersbook_id = 1000dangersbook.id
        join user
          on 1000dangersbook.user_id = :user_id
        where
          papconnection.1000dangersbook_id = (select max(1000dangersbook.id) from 1000dangersbook) 
          and 1000dangersbook.name = :1000dangersbook_name');
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':1000dangersbook_name', $_SESSION['1000dangersbook_name']);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}
function savePapElements($papElements)
{
    global $conn;
    $statement = $conn->prepare('
        insert into 1000dangersbook(user_id, name, creationdate) values(:user_id, :1000dangersbook_name, now())');
    $statement->bindParam(':1000dangersbook_name', $_SESSION['1000dangersbook_name']);
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->execute();
    if(is_null($papElements))
    {
        return;
    }
    foreach($papElements as $papElement)
    {
        $statement = $conn->prepare('
            insert into papelement
              (id, 1000dangersbook_id, x, y, paptype_id, title, text)
            values (:id, (select max(1000dangersbook.id) from 1000dangersbook where 1000dangersbook.name = :1000dangersbook_name), :x, :y, (select id from paptype where paptype.name = :type), :title, :text)');
        $statement->bindParam(':id', $papElement['id']);
        $statement->bindParam(':1000dangersbook_name', $_SESSION['1000dangersbook_name']);
        $statement->bindParam(':x', $papElement['x']);
        $statement->bindParam(':y', $papElement['y']);
        $statement->bindParam(':type', $papElement['type']);
        $statement->bindParam(':title', $papElement['title']);
        $statement->bindParam(':text', $papElement['text']);
        $result = $statement->execute();
    }
}
function savePapConnections($papConnections)
{
    //TODO  savePapElements() muss immer vorher aufgerufen werden!!
    global $conn;
    if(is_null($papConnections))
    {
        return;
    }
    foreach($papConnections as $papConnection)
    {
        $statement = $conn->prepare('
            insert into papconnection
              (source_id, destination_id, 1000dangersbook_id, source_offset_x, source_offset_y, destination_offset_x, destination_offset_y, title)
            values (:source_id, :destination_id, (select max(1000dangersbook.id) from 1000dangersbook where 1000dangersbook.name = :1000dangersbook_name), :source_offset_x, :source_offset_y, :destination_offset_x, :destination_offset_y, :title)');
        $statement->bindParam(':source_id', $papConnection['source_id']);
        $statement->bindParam(':destination_id', $papConnection['destination_id']);
        $statement->bindParam(':1000dangersbook_name', $_SESSION['1000dangersbook_name']);
        $statement->bindParam(':source_offset_x', $papConnection['source_offset_x']);
        $statement->bindParam(':source_offset_y', $papConnection['source_offset_y']);
        $statement->bindParam(':destination_offset_x', $papConnection['destination_offset_x']);
        $statement->bindParam(':destination_offset_y', $papConnection['destination_offset_y']);
        $statement->bindParam(':title', $papConnection['title']);
        $result = $statement->execute();
    }
}
function getTeamName($user_id)
{
    global $conn;
    $statement = $conn->prepare('
        select name
        from user
        where id = :user_id');
    $statement->bindParam(':user_id', $user_id);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    if(count($rows) === 0)
    {
        return false;
    }
    return $rows[0]['name'];
}
function getBooks($user_id)
{
    global $conn;
    $statement = $conn->prepare('
        select 1000dangersbook.name
        from 1000dangersbook
        join user on 1000dangersbook.user_id = user.id
        where user.id = :user_id
        group by 1000dangersbook.name');
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
        select 1000dangersbook.id
        from 1000dangersbook
        join user
          on 1000dangersbook.user_id = user.id
        where 1000dangersbook.name = :1000dangersbook_name
          and user.id = :user_id
        limit 1');  
    $statement->bindParam(':1000dangersbook_name', $bookName);
    $statement->bindParam(':user_id', $user_id);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return count($rows) > 0;
}
function createNewBook($user_id, $bookName)
{
    global $conn;
    $statement = $conn->prepare('
        insert into 1000dangersbook(user_id, name, creationdate) values(:user_id, :1000dangersbook_name, now())');
    $statement->bindParam(':user_id', $user_id);
    $statement->bindParam(':1000dangersbook_name', $bookName);
    $statement->execute();
    $statement = $conn->prepare('
        insert into papelement(id, 1000dangersbook_id, paptype_id, x, y, title, text) values(1, (select max(id) from 1000dangersbook where name = :1000dangersbook_name and user_id = :user_id), 1, 100, 100, "Title", "Texte")');
    $statement->bindParam(':1000dangersbook_name', $bookName);
    $statement->bindParam(':user_id', $user_id);
    $statement->execute();
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



