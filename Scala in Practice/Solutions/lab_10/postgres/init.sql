-- postgres/init.sql
CREATE TABLE students(
  index INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  year SMALLINT NOT NULL
);

INSERT INTO students (index, name, year)
VALUES
  (331826, 'Chris Cornell', 1),
  (332137, 'Kurt Cobain', 2),
  (334200, 'Peter Steele', 3),
  (330666, 'Layne Staley', 1),
  (336969, 'Mathew Sanders', 2),
  (335431, 'Maynard Keenan', 3),
  (331234, 'Trent Reznor', 1),
  (330091, 'Corey Taylor', 2),
  (330458, 'Phil Anselmo', 3),
  (331142, 'Ian Curtis', 1);

CREATE TABLE lectures(
  id VARCHAR(5) PRIMARY KEY,
  name TEXT NOT NULL
);

INSERT INTO lectures (id, name)
VALUES
  ('SiP', 'Scala in Practice'),
  ('SO', 'Systemy Operacyjne'),
  ('BD', 'Bazy Danych'),
  ('WEPPO', 'Wybrane Elementy Praktyki Projektowania Oprogramowania');

CREATE TABLE enrollments(
  lecture_id VARCHAR(5) REFERENCES lectures(id),
  student_id INTEGER REFERENCES students(index),

  PRIMARY KEY (lecture_id, student_id)
);

INSERT INTO enrollments (lecture_id, student_id)
VALUES
  ('SiP', 331826),
  ('SiP', 331234),
  ('SiP', 332137),
  ('SiP', 330666),
  ('SiP', 334200),
  ('SO', 331826),
  ('SO', 331142),
  ('SO', 336969),
  ('SO', 335431),
  ('WEPPO', 331826),
  ('WEPPO', 336969),
  ('WEPPO', 335431),
  ('BD', 331826);
