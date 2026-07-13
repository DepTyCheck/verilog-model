-- Seed: 8099364962243640665,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vqnnmhh is
  port ( beohvvm : buffer std_logic_vector(3 to 0)
  ; svxcmstckc : linkage std_logic
  ; zvpr : linkage integer_vector(0 downto 0)
  ; cyhqldvr : inout physical_subtype_mirror
  );
end vqnnmhh;

architecture xun of vqnnmhh is
  
begin
  -- Multi-driven assignments
  beohvvm <= (others => '0');
  beohvvm <= (others => '0');
  beohvvm <= beohvvm;
end xun;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zcfu is
  port (trnhcdkd : inout array_subtype_mirror; mz : buffer integer; ux : out std_logic_vector(0 to 2); lspmhhf : linkage real);
end zcfu;

architecture rcpbbperxu of zcfu is
  
begin
  -- Single-driven assignments
  mz <= mz;
end rcpbbperxu;

entity sfcglohe is
  port (fvcppih : in integer; zitw : buffer real; cynnaanyvr : linkage bit; aucraqamwo : out real);
end sfcglohe;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zk of sfcglohe is
  shared variable u : physical_subtype_mirror;
  signal jnaqiaixl : integer_vector(0 downto 0);
  signal mou : std_logic;
  shared variable zt : physical_subtype_mirror;
  signal djg : integer_vector(0 downto 0);
  signal shfnk : std_logic;
  signal hnouyacs : std_logic_vector(3 to 0);
begin
  neflmury : entity work.vqnnmhh
    port map (beohvvm => hnouyacs, svxcmstckc => shfnk, zvpr => djg, cyhqldvr => zt);
  zikhnr : entity work.vqnnmhh
    port map (beohvvm => hnouyacs, svxcmstckc => mou, zvpr => jnaqiaixl, cyhqldvr => u);
  
  -- Multi-driven assignments
  shfnk <= '0';
  hnouyacs <= hnouyacs;
  mou <= shfnk;
  shfnk <= shfnk;
end zk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zifenaotqu is
  port ( dedemubscp : inout integer_vector(4 to 2)
  ; yjhlhsewru : inout enumeration_value_mirror
  ; ddc : linkage std_logic_vector(3 downto 0)
  ; apaavrtz : out time
  );
end zifenaotqu;

architecture zocgpi of zifenaotqu is
  signal mfckbwrk : real;
  signal zac : bit;
  signal dzvl : real;
  signal bytg : real;
  signal da : bit;
  signal fdfwj : real;
  signal d : real;
  signal rbq : bit;
  signal mvjkbtoyig : real;
  signal yuil : integer;
begin
  ymfjif : entity work.sfcglohe
    port map (fvcppih => yuil, zitw => mvjkbtoyig, cynnaanyvr => rbq, aucraqamwo => d);
  ffnoubyjrg : entity work.sfcglohe
    port map (fvcppih => yuil, zitw => fdfwj, cynnaanyvr => da, aucraqamwo => bytg);
  xplzh : entity work.sfcglohe
    port map (fvcppih => yuil, zitw => dzvl, cynnaanyvr => zac, aucraqamwo => mfckbwrk);
  
  -- Single-driven assignments
  apaavrtz <= 0 ns;
  yuil <= yuil;
  dedemubscp <= dedemubscp;
end zocgpi;



-- Seed after: 10513916605858738683,3566912872917928779
