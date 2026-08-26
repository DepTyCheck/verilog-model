-- Seed: 5699201897211625004,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity iyihi is
  port (dbykonjt : linkage integer_vector(3 downto 4); apmameoy : inout std_logic_vector(4 downto 1); ruae : inout integer);
end iyihi;

architecture dsoxskapwo of iyihi is
  
begin
  -- Single-driven assignments
  ruae <= 0_3;
  
  -- Multi-driven assignments
  apmameoy <= apmameoy;
  apmameoy <= "X10Z";
end dsoxskapwo;

entity ac is
  port (ukyun : out integer; rxejxuiz : in real; wlkcyeikrk : inout real);
end ac;

library ieee;
use ieee.std_logic_1164.all;

architecture hty of ac is
  signal erz : integer_vector(3 downto 4);
  signal ezzrmd : integer;
  signal tykvbo : integer_vector(3 downto 4);
  signal jlpm : integer;
  signal tw : integer_vector(3 downto 4);
  signal hvawfj : integer;
  signal rnwzekqni : std_logic_vector(4 downto 1);
  signal fp : integer_vector(3 downto 4);
begin
  qx : entity work.iyihi
    port map (dbykonjt => fp, apmameoy => rnwzekqni, ruae => hvawfj);
  qdnl : entity work.iyihi
    port map (dbykonjt => tw, apmameoy => rnwzekqni, ruae => jlpm);
  rcdt : entity work.iyihi
    port map (dbykonjt => tykvbo, apmameoy => rnwzekqni, ruae => ezzrmd);
  oiusymoahn : entity work.iyihi
    port map (dbykonjt => erz, apmameoy => rnwzekqni, ruae => ukyun);
  
  -- Single-driven assignments
  wlkcyeikrk <= wlkcyeikrk;
  
  -- Multi-driven assignments
  rnwzekqni <= ('0', 'U', 'X', 'W');
  rnwzekqni <= rnwzekqni;
  rnwzekqni <= rnwzekqni;
end hty;



-- Seed after: 10058601452886312497,6000118208082478503
