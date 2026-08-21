-- Seed: 8072008264709703125,16188444798499499427

entity ovhm is
  port (bfiw : linkage integer; pmwha : out integer; yggv : buffer time_vector(0 downto 4));
end ovhm;

architecture xjlcxzk of ovhm is
  
begin
  -- Single-driven assignments
  yggv <= yggv;
  pmwha <= 42;
end xjlcxzk;

entity xmts is
  port (rcetrb : out time; so : in time);
end xmts;

architecture t of xmts is
  
begin
  -- Single-driven assignments
  rcetrb <= so;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity wpeufxp is
  port (dxquriakdy : linkage std_logic_vector(0 downto 1));
end wpeufxp;

architecture jof of wpeufxp is
  signal zqljnqud : time_vector(0 downto 4);
  signal tlpxogts : integer;
  signal i : integer;
  signal zhxdzkrsx : time_vector(0 downto 4);
  signal otadxg : integer;
  signal icdm : integer;
  signal bnvwq : time;
  signal idiikcpyy : time;
begin
  unppmn : entity work.xmts
    port map (rcetrb => idiikcpyy, so => bnvwq);
  m : entity work.ovhm
    port map (bfiw => icdm, pmwha => otadxg, yggv => zhxdzkrsx);
  r : entity work.ovhm
    port map (bfiw => i, pmwha => tlpxogts, yggv => zqljnqud);
  
  -- Single-driven assignments
  bnvwq <= idiikcpyy;
end jof;



-- Seed after: 15610302470839557036,16188444798499499427
