-- Seed: 18196939427798711551,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity erfr is
  port (kmjwxirdk : in time; b : buffer real; qfg : out std_logic_vector(0 to 0));
end erfr;

architecture p of erfr is
  
begin
  -- Single-driven assignments
  b <= 16#4094.C#;
  
  -- Multi-driven assignments
  qfg <= "0";
  qfg <= (others => '-');
  qfg <= "0";
end p;

entity igo is
  port (psuwicls : in real_vector(3 to 0));
end igo;

library ieee;
use ieee.std_logic_1164.all;

architecture kkzoswzu of igo is
  signal rcfz : std_logic_vector(0 to 0);
  signal cdurjrvlvq : real;
  signal aijerqlo : time;
begin
  kmjqjl : entity work.erfr
    port map (kmjwxirdk => aijerqlo, b => cdurjrvlvq, qfg => rcfz);
  
  -- Single-driven assignments
  aijerqlo <= aijerqlo;
  
  -- Multi-driven assignments
  rcfz <= rcfz;
end kkzoswzu;

library ieee;
use ieee.std_logic_1164.all;

entity aqhqjdxges is
  port (mwgplkqoa : buffer time; jbfhx : inout std_logic_vector(1 to 4); bvcpsjhtpo : in time);
end aqhqjdxges;

architecture ynjslj of aqhqjdxges is
  signal wgtuhtlaj : real_vector(3 to 0);
begin
  bkygwnwyb : entity work.igo
    port map (psuwicls => wgtuhtlaj);
  
  -- Single-driven assignments
  wgtuhtlaj <= wgtuhtlaj;
  mwgplkqoa <= 8#0# ps;
  
  -- Multi-driven assignments
  jbfhx <= "H0ZZ";
  jbfhx <= ('W', 'H', '0', 'L');
  jbfhx <= "UWWL";
end ynjslj;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (hhpftdg : inout time; dckz : buffer std_logic_vector(3 downto 1));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture ywchhszf of f is
  signal px : time;
  signal dxebudj : std_logic_vector(1 to 4);
  signal qzre : time;
  signal fnd : real_vector(3 to 0);
  signal hicmymfs : time;
  signal mfkmyurl : std_logic_vector(1 to 4);
begin
  xtxhq : entity work.aqhqjdxges
    port map (mwgplkqoa => hhpftdg, jbfhx => mfkmyurl, bvcpsjhtpo => hicmymfs);
  r : entity work.igo
    port map (psuwicls => fnd);
  mypcub : entity work.aqhqjdxges
    port map (mwgplkqoa => qzre, jbfhx => dxebudj, bvcpsjhtpo => px);
  
  -- Single-driven assignments
  px <= hhpftdg;
  hicmymfs <= 2#1_0# ns;
  fnd <= fnd;
  
  -- Multi-driven assignments
  mfkmyurl <= "W-UZ";
  mfkmyurl <= dxebudj;
  mfkmyurl <= ('H', 'U', 'W', '1');
end ywchhszf;



-- Seed after: 17026916823414224389,15306146742274979323
