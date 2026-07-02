-- Seed: 13061084072445034397,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity wzy is
  port (ccjtad : out std_logic_vector(1 downto 4); tjd : linkage std_logic; xm : linkage std_logic; tleab : buffer real);
end wzy;

architecture xrueynj of wzy is
  
begin
  -- Multi-driven assignments
  ccjtad <= (others => '0');
  ccjtad <= "";
  ccjtad <= "";
end xrueynj;

library ieee;
use ieee.std_logic_1164.all;

entity ttrfsi is
  port (fjdap : inout std_logic_vector(3 to 2));
end ttrfsi;

library ieee;
use ieee.std_logic_1164.all;

architecture vrfamyk of ttrfsi is
  signal nvsykucmf : real;
  signal sf : std_logic;
  signal fuwj : std_logic;
begin
  adkah : entity work.wzy
    port map (ccjtad => fjdap, tjd => fuwj, xm => sf, tleab => nvsykucmf);
end vrfamyk;

entity dgbvvl is
  port (ywkh : inout time; ohrzknb : buffer integer; hhdqnlxyh : out bit_vector(2 to 4));
end dgbvvl;

library ieee;
use ieee.std_logic_1164.all;

architecture pwjaiqhq of dgbvvl is
  signal qwddtzra : real;
  signal lenycx : std_logic;
  signal jqvcxv : std_logic;
  signal csptifvj : std_logic_vector(1 downto 4);
  signal zbaedf : real;
  signal ywmdbweg : std_logic;
  signal x : std_logic_vector(1 downto 4);
begin
  v : entity work.ttrfsi
    port map (fjdap => x);
  j : entity work.ttrfsi
    port map (fjdap => x);
  qzamkrap : entity work.wzy
    port map (ccjtad => x, tjd => ywmdbweg, xm => ywmdbweg, tleab => zbaedf);
  dstgy : entity work.wzy
    port map (ccjtad => csptifvj, tjd => jqvcxv, xm => lenycx, tleab => qwddtzra);
  
  -- Single-driven assignments
  ywkh <= 0 min;
  ohrzknb <= 2;
  hhdqnlxyh <= ('0', '1', '0');
  
  -- Multi-driven assignments
  x <= (others => '0');
  x <= (others => '0');
end pwjaiqhq;

entity lnzfguont is
  port (wqkd : buffer time; o : out boolean_vector(1 downto 0); nhmrwyo : out character; epuvhs : buffer real_vector(0 to 2));
end lnzfguont;

library ieee;
use ieee.std_logic_1164.all;

architecture eydzwrl of lnzfguont is
  signal gdueocverj : real;
  signal hvjvqefrd : std_logic;
  signal bzy : real;
  signal v : std_logic;
  signal zxoh : std_logic_vector(1 downto 4);
begin
  tjiocji : entity work.wzy
    port map (ccjtad => zxoh, tjd => v, xm => v, tleab => bzy);
  gqt : entity work.wzy
    port map (ccjtad => zxoh, tjd => v, xm => hvjvqefrd, tleab => gdueocverj);
  
  -- Multi-driven assignments
  zxoh <= "";
  zxoh <= (others => '0');
end eydzwrl;



-- Seed after: 11177431561174468371,13694093582652240945
