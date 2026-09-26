-- Seed: 10813467990129027088,10875537289884587119

entity zqey is
  port (glj : out integer_vector(2 to 3); hfigdujpo : out integer; dsjflfwix : in character; zeelcohrew : buffer integer);
end zqey;

architecture kzbc of zqey is
  
begin
  -- Single-driven assignments
  glj <= glj;
end kzbc;

entity t is
  port (pl : out real_vector(0 to 4));
end t;

architecture fc of t is
  signal zwodcjhla : integer;
  signal hxhsftv : integer;
  signal exlcceki : integer_vector(2 to 3);
  signal d : integer;
  signal qpkulw : integer;
  signal tbsshdpbgi : integer_vector(2 to 3);
  signal penw : integer;
  signal ocxq : character;
  signal zrnxj : integer;
  signal ddawdjcrgd : integer_vector(2 to 3);
begin
  pdz : entity work.zqey
    port map (glj => ddawdjcrgd, hfigdujpo => zrnxj, dsjflfwix => ocxq, zeelcohrew => penw);
  u : entity work.zqey
    port map (glj => tbsshdpbgi, hfigdujpo => qpkulw, dsjflfwix => ocxq, zeelcohrew => d);
  a : entity work.zqey
    port map (glj => exlcceki, hfigdujpo => hxhsftv, dsjflfwix => ocxq, zeelcohrew => zwodcjhla);
  
  -- Single-driven assignments
  pl <= pl;
  ocxq <= ocxq;
end fc;

library ieee;
use ieee.std_logic_1164.all;

entity gr is
  port (jnyjjdmy : out std_logic_vector(1 to 4));
end gr;

architecture kpanhxeskp of gr is
  signal upcyi : integer;
  signal vpjxwghh : character;
  signal epwq : integer;
  signal q : integer_vector(2 to 3);
  signal sucp : integer;
  signal aib : character;
  signal dd : integer;
  signal iytoqqme : integer_vector(2 to 3);
begin
  wckeqrj : entity work.zqey
    port map (glj => iytoqqme, hfigdujpo => dd, dsjflfwix => aib, zeelcohrew => sucp);
  djr : entity work.zqey
    port map (glj => q, hfigdujpo => epwq, dsjflfwix => vpjxwghh, zeelcohrew => upcyi);
  
  -- Single-driven assignments
  vpjxwghh <= 'o';
  
  -- Multi-driven assignments
  jnyjjdmy <= ('1', 'L', 'U', 'Z');
  jnyjjdmy <= ('0', 'W', 'H', 'W');
  jnyjjdmy <= jnyjjdmy;
  jnyjjdmy <= jnyjjdmy;
end kpanhxeskp;

library ieee;
use ieee.std_logic_1164.all;

entity ogrqhvy is
  port (ohpivzowsu : in std_logic_vector(1 to 0); frdhjxcko : inout integer; khhhs : inout boolean_vector(1 downto 3));
end ogrqhvy;

library ieee;
use ieee.std_logic_1164.all;

architecture civihcm of ogrqhvy is
  signal zhxo : real_vector(0 to 4);
  signal lluqkra : integer;
  signal p : character;
  signal xp : integer;
  signal unf : integer_vector(2 to 3);
  signal dfjf : std_logic_vector(1 to 4);
begin
  epbw : entity work.gr
    port map (jnyjjdmy => dfjf);
  vqme : entity work.zqey
    port map (glj => unf, hfigdujpo => xp, dsjflfwix => p, zeelcohrew => lluqkra);
  xdhnic : entity work.t
    port map (pl => zhxo);
  
  -- Single-driven assignments
  p <= p;
  khhhs <= (others => TRUE);
end civihcm;



-- Seed after: 11942104461089835740,10875537289884587119
