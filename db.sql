
DROP TRIGGER IF EXISTS chat_update;
DROP TRIGGER IF EXISTS chat_insert;
DROP VIEW IF EXISTS chat;
DROP TABLE IF EXISTS hidden_messages;
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS users;

CREATE TABLE IF NOT EXISTS users(
             u_id INTEGER PRIMARY KEY AUTOINCREMENT,
             username TEXT NOT NULL,
             password TEXT NOT NULL
             );

CREATE TABLE IF NOT EXISTS messages(
           m_id integer PRIMARY KEY,
           m_uid integer,
           post_time datetime DEFAULT current_timestamp,
           body TEXT NOT NULL,
           FOREIGN KEY (m_uid) REFERENCES users(u_id)
              ON DELETE SET NULL
           );

CREATE TABLE IF NOT EXISTS hidden_messages(
           hm_id integer PRIMARY KEY,
           hm_uid integer NOT NULL,
           hm_mid integer NOT NULL,
           FOREIGN KEY (hm_uid) REFERENCES users(u_id)
              ON DELETE CASCADE,
           FOREIGN KEY (hm_mid) REFERENCES messages(m_id)
              ON DELETE CASCADE
           );

CREATE VIEW IF NOT EXISTS chat AS
  SELECT  username AS c_username,
          post_time AS c_post_time,
          body AS c_body,
          users.u_id AS c_uid,
          messages.m_id AS c_mid FROM users
  INNER JOIN messages ON users.u_id = messages.m_uid;

CREATE TRIGGER IF NOT EXISTS chat_insert INSTEAD OF INSERT ON chat
BEGIN
  INSERT INTO messages (m_uid, body) VALUES (NEW.c_uid, NEW.c_body);
END;

CREATE TRIGGER IF NOT EXISTS chat_update INSTEAD OF UPDATE ON chat
BEGIN
  UPDATE messages SET body=NEW.c_body where m_id=NEW.c_mid;
END;
