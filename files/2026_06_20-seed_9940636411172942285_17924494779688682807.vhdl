-- Seed: 9940636411172942285,17924494779688682807

entity sb is
  port (f : out bit; orz : in character);
end sb;

architecture daxbdjdzi of sb is
  
begin
  -- Single-driven assignments
  f <= '0';
end daxbdjdzi;

entity x is
  port (biugqnt : in real; zhnrek : out severity_level);
end x;

architecture ccvcohhn of x is
  signal dsrzmdtx : character;
  signal qvjqrlo : bit;
  signal p : character;
  signal diagfqlx : bit;
begin
  o : entity work.sb
    port map (f => diagfqlx, orz => p);
  dtensd : entity work.sb
    port map (f => qvjqrlo, orz => dsrzmdtx);
  
  -- Single-driven assignments
  zhnrek <= WARNING;
end ccvcohhn;

library ieee;
use ieee.std_logic_1164.all;

entity rxfvfa is
  port (jzqasu : inout real; ajk : inout std_logic);
end rxfvfa;

architecture ssaypkam of rxfvfa is
  signal bqrnwsxe : severity_level;
  signal fy : bit;
  signal fbvswjoyi : character;
  signal zgykcvic : bit;
begin
  xdwqrvpy : entity work.sb
    port map (f => zgykcvic, orz => fbvswjoyi);
  mbunhy : entity work.sb
    port map (f => fy, orz => fbvswjoyi);
  gfrnc : entity work.x
    port map (biugqnt => jzqasu, zhnrek => bqrnwsxe);
  
  -- Single-driven assignments
  fbvswjoyi <= 'a';
  
  -- Multi-driven assignments
  ajk <= '0';
end ssaypkam;

library ieee;
use ieee.std_logic_1164.all;

entity cqbudznhza is
  port (juvsspnti : in time; bunsifn : inout severity_level; quimvua : in std_logic_vector(3 to 3));
end cqbudznhza;

library ieee;
use ieee.std_logic_1164.all;

architecture dttd of cqbudznhza is
  signal ri : bit;
  signal mcnautf : std_logic;
  signal klkpony : character;
  signal pijow : bit;
  signal cqioemabuu : severity_level;
  signal ssn : real;
begin
  pway : entity work.x
    port map (biugqnt => ssn, zhnrek => cqioemabuu);
  rxgvl : entity work.sb
    port map (f => pijow, orz => klkpony);
  uok : entity work.rxfvfa
    port map (jzqasu => ssn, ajk => mcnautf);
  pgh : entity work.sb
    port map (f => ri, orz => klkpony);
  
  -- Single-driven assignments
  bunsifn <= ERROR;
  klkpony <= 'x';
  
  -- Multi-driven assignments
  mcnautf <= '0';
  mcnautf <= 'L';
  mcnautf <= 'Z';
end dttd;



-- Seed after: 13196163610028895891,17924494779688682807
