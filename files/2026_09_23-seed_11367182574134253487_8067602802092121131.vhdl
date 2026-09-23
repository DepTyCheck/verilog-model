-- Seed: 11367182574134253487,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity hfmm is
  port (cgoldj : linkage std_logic; lppqh : out time; zibei : in integer);
end hfmm;

architecture hdv of hfmm is
  
begin
  -- Single-driven assignments
  lppqh <= lppqh;
end hdv;

entity rjbf is
  port (rrxzsusjc : linkage bit_vector(3 to 1); fjhbatwdef : buffer time; tbcvnueuqs : linkage real; gpyofxeb : buffer integer);
end rjbf;

library ieee;
use ieee.std_logic_1164.all;

architecture wrbmdktw of rjbf is
  signal yfaph : time;
  signal rza : integer;
  signal vdaco : time;
  signal izju : time;
  signal nrzsh : std_logic;
  signal laopbymt : integer;
  signal tjblgwsqp : std_logic;
begin
  ico : entity work.hfmm
    port map (cgoldj => tjblgwsqp, lppqh => fjhbatwdef, zibei => laopbymt);
  ykxciw : entity work.hfmm
    port map (cgoldj => nrzsh, lppqh => izju, zibei => laopbymt);
  lbvck : entity work.hfmm
    port map (cgoldj => tjblgwsqp, lppqh => vdaco, zibei => rza);
  gatgxlzmk : entity work.hfmm
    port map (cgoldj => nrzsh, lppqh => yfaph, zibei => gpyofxeb);
  
  -- Single-driven assignments
  gpyofxeb <= 02;
  
  -- Multi-driven assignments
  tjblgwsqp <= 'X';
  tjblgwsqp <= 'Z';
  tjblgwsqp <= '1';
end wrbmdktw;

library ieee;
use ieee.std_logic_1164.all;

entity mlctesjes is
  port (abmvre : buffer time; tknwybotjt : in real; dtcwmizgme : in std_logic_vector(2 downto 1));
end mlctesjes;

architecture zzbvvjdi of mlctesjes is
  signal u : integer;
  signal rdb : real;
  signal fqqn : time;
  signal dy : bit_vector(3 to 1);
begin
  udhjm : entity work.rjbf
    port map (rrxzsusjc => dy, fjhbatwdef => fqqn, tbcvnueuqs => rdb, gpyofxeb => u);
  
  -- Single-driven assignments
  abmvre <= abmvre;
end zzbvvjdi;



-- Seed after: 7272113151460587845,8067602802092121131
