-- Seed: 3533333375248083993,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity uezvazttyt is
  port (ksimmf : in std_logic_vector(1 downto 2); dceh : linkage severity_level; w : in time);
end uezvazttyt;

architecture fgx of uezvazttyt is
  
begin
  
end fgx;

entity tjxo is
  port (psxztx : out integer_vector(3 downto 2); nfpyyu : buffer real; ry : out real);
end tjxo;

library ieee;
use ieee.std_logic_1164.all;

architecture gufmgv of tjxo is
  signal cncomwuvp : time;
  signal v : severity_level;
  signal gbw : std_logic_vector(1 downto 2);
begin
  iirwspbsaw : entity work.uezvazttyt
    port map (ksimmf => gbw, dceh => v, w => cncomwuvp);
  
  -- Single-driven assignments
  nfpyyu <= 3_4_0_4_3.3;
  psxztx <= psxztx;
  ry <= 21.2;
  cncomwuvp <= cncomwuvp;
  
  -- Multi-driven assignments
  gbw <= gbw;
  gbw <= gbw;
end gufmgv;

library ieee;
use ieee.std_logic_1164.all;

entity bl is
  port (bhy : inout std_logic_vector(2 downto 3); i : out real);
end bl;

library ieee;
use ieee.std_logic_1164.all;

architecture hn of bl is
  signal qthbfcyhvd : time;
  signal mciwikzfgk : severity_level;
  signal chm : std_logic_vector(1 downto 2);
  signal setmzyi : time;
  signal j : severity_level;
  signal xlqxdb : time;
  signal rvbwiwn : severity_level;
  signal ijdan : std_logic_vector(1 downto 2);
  signal lvnn : real;
  signal jlywmifixj : integer_vector(3 downto 2);
begin
  ms : entity work.tjxo
    port map (psxztx => jlywmifixj, nfpyyu => lvnn, ry => i);
  rkbmjzgli : entity work.uezvazttyt
    port map (ksimmf => ijdan, dceh => rvbwiwn, w => xlqxdb);
  il : entity work.uezvazttyt
    port map (ksimmf => bhy, dceh => j, w => setmzyi);
  drx : entity work.uezvazttyt
    port map (ksimmf => chm, dceh => mciwikzfgk, w => qthbfcyhvd);
  
  -- Single-driven assignments
  setmzyi <= xlqxdb;
  xlqxdb <= xlqxdb;
  qthbfcyhvd <= setmzyi;
  
  -- Multi-driven assignments
  chm <= "";
  chm <= (others => '0');
  bhy <= (others => '0');
  ijdan <= (others => '0');
end hn;



-- Seed after: 1212469808929858228,10975587770517081373
