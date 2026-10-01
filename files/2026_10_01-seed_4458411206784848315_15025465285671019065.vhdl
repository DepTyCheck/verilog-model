-- Seed: 4458411206784848315,15025465285671019065

entity tpuo is
  port (pozicxz : inout boolean_vector(1 downto 1); xolwlqoe : buffer integer; omogxc : out integer; f : linkage boolean_vector(2 to 2));
end tpuo;

architecture jnzvc of tpuo is
  
begin
  -- Single-driven assignments
  omogxc <= xolwlqoe;
end jnzvc;

library ieee;
use ieee.std_logic_1164.all;

entity uvohera is
  port (emwfwhd : in std_logic_vector(4 downto 3); gphy : inout boolean);
end uvohera;

architecture jhqxhvjhq of uvohera is
  signal zekzk : boolean_vector(2 to 2);
  signal aj : integer;
  signal aryvwpwf : integer;
  signal aytyhsuziv : boolean_vector(1 downto 1);
  signal uszu : boolean_vector(2 to 2);
  signal c : integer;
  signal ykepo : integer;
  signal m : boolean_vector(1 downto 1);
  signal hgbaueqxlf : boolean_vector(2 to 2);
  signal eejamjlsn : integer;
  signal qwbnsilpjd : integer;
  signal kmmxt : boolean_vector(1 downto 1);
  signal ph : boolean_vector(2 to 2);
  signal jqiwtuy : integer;
  signal pbg : integer;
  signal mwdzrprwl : boolean_vector(1 downto 1);
begin
  ahqtc : entity work.tpuo
    port map (pozicxz => mwdzrprwl, xolwlqoe => pbg, omogxc => jqiwtuy, f => ph);
  yuhbclpgru : entity work.tpuo
    port map (pozicxz => kmmxt, xolwlqoe => qwbnsilpjd, omogxc => eejamjlsn, f => hgbaueqxlf);
  vohonkn : entity work.tpuo
    port map (pozicxz => m, xolwlqoe => ykepo, omogxc => c, f => uszu);
  w : entity work.tpuo
    port map (pozicxz => aytyhsuziv, xolwlqoe => aryvwpwf, omogxc => aj, f => zekzk);
  
  -- Single-driven assignments
  gphy <= gphy;
end jhqxhvjhq;



-- Seed after: 4054175620681388079,15025465285671019065
