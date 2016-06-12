create database if not exists 1998294_db;
use 1998294_db;
drop table if exists fc_arrow;
drop table if exists offset_location;
drop table if exists location;
drop table if exists fc_shape;
drop table if exists fc_shape_type;
drop table if exists book;
drop table if exists user;
create table user(id int auto_increment primary key, name varchar(50), password varchar(50));
create table book(id int auto_increment primary key, user_id int, name varchar(150), creation_date datetime, foreign key(user_id) references user(id));
create table fc_shape_type(id int auto_increment primary key, name varchar(30));
create table fc_shape(id int, book_id int, fc_shape_type_id int, x int, y int, title varchar(200), text text, primary key(id, book_id), foreign key(book_id) references book(id), foreign key(fc_shape_type_id) references fc_shape_type(id));

create table fc_arrow(id int, book_id int, source_id int null, destination_id int null, source_offset_x int, source_offset_y int, destination_offset_x int, destination_offset_y int, title varchar(200), text text,
    primary key(id, book_id), foreign key(source_id) references fc_shape(id), foreign key(destination_id) references fc_shape(id));

insert into user(id, name, password) values(1, "rene", "1234");
insert into book(id, user_id, name, creation_date) values(1, 1, "1000 Gefahren", now());
insert into fc_shape_type(id, name) values(1, "Start"), (2, "End"), (3, "Action"), (4, "Condition");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(1, 1, 1, 517, 6, "Start", "");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(2, 1, 2, 535, 525, "Ende", "");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(3, 1, 3, 502, 159, "Insel gestrandet", "Du hast Schiffbruch erlitten. Nachdem du dich auf eine einsame Insel gerettet hast, begegnen dir 5 Kannibalen.");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(4, 1, 4, 797, 85, "Kannibalen kennenlernen?", "Moechtest du dich erstmal mit den Kannibalen anfreunden?");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(5, 1, 3, 1015, 333, "Vor Kannibalen fliehen", "Du hast dich dazu entschieden, vor, vor den Kannibalen zu fliehen. Auf der Flucht stolperst du und verblutest qualvoll.");
insert into fc_shape(id, book_id, fc_shape_type_id, x, y, title, text) values(6, 1, 3, 586, 398, "Mit Kannibalen anfreunden", "Du lernst die Kannibalen kennen. Sie sind viel netter, als man sonst so liest. Sie organisieren fuer dich die Heimreise. 6 Wochen spaeter bist du zu Hause.");
insert into fc_arrow(id, book_id, source_id, destination_id, source_offset_x, source_offset_y, destination_offset_x, destination_offset_y, title) values(1, 1, null, 2, 10, 20, 20,40, "ja");
insert into fc_arrow(id, book_id, source_id, destination_id, source_offset_x, source_offset_y, destination_offset_x, destination_offset_y, title) values(1, 1, 1, 2, 10, 20, 20,40, "ja");


















