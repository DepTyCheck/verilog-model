-- Seed: 13073841340336886910,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity tcx is
  port (zmofzqs : linkage std_logic_vector(1 downto 2); xjz : linkage time_vector(0 downto 3); ijnaeqbrj : inout integer_vector(3 downto 3));
end tcx;

architecture lagkrzr of tcx is
  
begin
  
end lagkrzr;

entity vclk is
  port (hvthvyka : buffer time; c : inout time; wcpo : linkage real; lcycdgcw : in integer);
end vclk;

library ieee;
use ieee.std_logic_1164.all;

architecture wlaypvxhgf of vclk is
  signal nbikzjqbdh : integer_vector(3 downto 3);
  signal zelzow : time_vector(0 downto 3);
  signal ppouustxs : std_logic_vector(1 downto 2);
  signal njurvguot : integer_vector(3 downto 3);
  signal csqsyajhz : time_vector(0 downto 3);
  signal mnny : std_logic_vector(1 downto 2);
begin
  lqdahsyhtu : entity work.tcx
    port map (zmofzqs => mnny, xjz => csqsyajhz, ijnaeqbrj => njurvguot);
  akb : entity work.tcx
    port map (zmofzqs => ppouustxs, xjz => zelzow, ijnaeqbrj => nbikzjqbdh);
  
  -- Single-driven assignments
  c <= hvthvyka;
  hvthvyka <= 16#2_1_2_2# us;
  
  -- Multi-driven assignments
  mnny <= mnny;
  mnny <= mnny;
  ppouustxs <= "";
  mnny <= mnny;
end wlaypvxhgf;

library ieee;
use ieee.std_logic_1164.all;

entity ffkhusf is
  port ( gfrcsqnr : out std_logic_vector(1 to 0)
  ; tswpcfs : buffer boolean
  ; phzaun : out std_logic_vector(3 downto 4)
  ; pn : linkage std_logic_vector(1 downto 0)
  );
end ffkhusf;

library ieee;
use ieee.std_logic_1164.all;

architecture xuvjrewlf of ffkhusf is
  signal j : integer_vector(3 downto 3);
  signal pug : time_vector(0 downto 3);
  signal sdpzclrc : std_logic_vector(1 downto 2);
  signal qpel : integer_vector(3 downto 3);
  signal fybjazma : time_vector(0 downto 3);
  signal mxgavqmhy : integer_vector(3 downto 3);
  signal hphr : time_vector(0 downto 3);
begin
  cixfpudypx : entity work.tcx
    port map (zmofzqs => phzaun, xjz => hphr, ijnaeqbrj => mxgavqmhy);
  rqmxgei : entity work.tcx
    port map (zmofzqs => phzaun, xjz => fybjazma, ijnaeqbrj => qpel);
  bttpmrwsch : entity work.tcx
    port map (zmofzqs => sdpzclrc, xjz => pug, ijnaeqbrj => j);
  
  -- Multi-driven assignments
  sdpzclrc <= "";
  phzaun <= phzaun;
  sdpzclrc <= (others => '0');
  phzaun <= phzaun;
end xuvjrewlf;



-- Seed after: 1948222682583165095,1112937151005418631
