<?php
    include_once("src/db.php");
    if(isset($_POST['btnLogout']))
    {
        session_unset();
    }
    $errorMessageLogin = "";
    $errorMessageCreateAccount = "";

    if(!isset($_SESSION['user_id']))
    {
        if(isset($_POST['btnLogin']))
        {
            $user_id = get_user_id($_POST['txtName'], $_POST['txtPassword']);
            if($user_id !== -1)
            {
                $_SESSION['user_id'] = $user_id;
            }
            else
            {
                $errorMessageLogin = "Benutzername oder Password falsch";
            }
        }
        if(isset($_POST['btnCreateAccount']))
        {
            create_user($_POST['txtNewName'], $_POST['txtNewPassword'], $_POST['txtNewEmail']);
        }
        if(!isset($_SESSION['user_id']))
        {
?>
<html>
    <body>
        <h1>Tausend Gefahren</h1>
        <br />
        <h2>Einloggen</h2>
        <form action="" method="post">
            <table border="0">
                <tr>
                    <td>Name</td>
                    <td>
                    <input type="text" name="txtName" value="<?php echo (isset($_POST['txtName']) ? $_POST['txtName'] : ''); ?>" />
                    </td>
                </tr>
                <tr>
                    <td>Passwort</td>
                    <td>
                        <input type="password" name="txtPassword" />
                    </td>
                </tr>
                <tr>
                    <td colspan="2">
                        <input type="submit" name="btnLogin" value="Einloggen" />
                    </td>
                </tr>
                <tr>
                <td colspan="2" style="color:red"><?php echo $errorMessageLogin; ?></td>
                </tr>
            </table>
        </form>
        <br />
        <h2>Neuen Account anlegen</h2>
        <form action="" method="post">
            <table border="0">
                <tr>
                    <td>Name</td>
                    <td>
                    <input type="text" name="txtNewName" value="<?php echo (isset($_POST['txtNewName']) ? $_POST['txtNewName'] : ''); ?>" />
                    </td>
                </tr>
                <tr>
                    <td>Passwort</td>
                    <td>
                        <input type="password" name="txtNewPassword" />
                    </td>
                </tr>
                <tr>
                    <td>Email</td>
                    <td>
                        <input type="text" name="txtNewEmail" />
                    </td>
                </tr>
                <tr>
                    <td colspan="2">
                        <input type="submit" name="btnCreateAccount" value="Account anlegen" />
                    </td>
                </tr>
                <tr>
                <td colspan="2" style="color:red"><?php echo $errorMessageCreateAccount; ?></td>
                </tr>
            </table>
        </form>
    </body>
</html>
<?php
        }
    }
    if(isset($_SESSION['user_id']))
    {
        if(isset($_POST['btnCreateNewBook']))
        {
            if(createNewBook($_SESSION['user_id'], $_POST['txtNewBookName']))
            {
                $_SESSION['book_name'] = $_POST['txtNewBookName'];
            }
        }
        if(isset($_POST['bookName']))
        {
            $_SESSION['book_name'] = $_POST['bookName'];
        }
        if(!isset($_SESSION['book_name']))
        {
?>
<html>
    <body>
        <h1>Tausend Gefahren</h1>
        <br />
        <h2>Buch auswählen</h2>
        <br />
        <form action="" method="post">
            <table border="0">
                <tr>
                    <td>
                        <select name="bookName" onchange="this.form.submit();">
                            <option>Bitte Buch auswählen</option>
<?php
    $books = getBooks($_SESSION['user_id']);
    foreach($books as $book)
    {
        echo '<option value="' . $book['name'] . '">' . $book['name'] . '</option>';
    }
?>
                        </select>
                    </td>
                </tr>
            </table>
        </form>
        <br />
        <br />
        <br />
        <h2>Neues Buch anlegen</h2>
        <br>
        <form action="" method="post">
            <input type="text" name="txtNewBookName" />
            <input type="submit" name="btnCreateNewBook" value="Buch anlegen" />
        </form>

    </body>
</html>
<?php
        }
    }
    if(isset($_SESSION['user_id']) && isset($_SESSION['book_name']))
    {
?>
<html style="background-color:green">
<body style="background-color:yellow;margin:0px">
<form action="" method="post">
    <input type="submit" name="btnLogout" value="Ausloggen" />
</form>
<a href=""
<?php
    echo $_SESSION['book_name'];
    echo "<br><br>";
    echo $_SESSION['user_id'];
?>
<script src="thousandDangers.js"></script>
<script>
    var node = document.getElementById('thousandDangers')
    var app = Elm.Main.fullscreen();
    app.ports.getScrollPosition.subscribe(function(id){
        var el = document.getElementById(id)
        return app.ports.scrollPositionTold.send({x : el.scrollLeft, y : el.scrollTop+el.top})
    })
</script>
</body>
</html>
<?php
    }
?>
