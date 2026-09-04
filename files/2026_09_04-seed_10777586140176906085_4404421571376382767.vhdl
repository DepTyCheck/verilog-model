-- Seed: 10777586140176906085,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity vjas is
  port (celkh : inout std_logic; mpfui : linkage std_logic_vector(0 to 3));
end vjas;

architecture jymwizy of vjas is
  
begin
  -- Multi-driven assignments
  celkh <= celkh;
  celkh <= 'H';
  celkh <= 'H';
end jymwizy;

library ieee;
use ieee.std_logic_1164.all;

entity paycknoacn is
  port (kndzbye : in boolean; khjhaoyn : linkage std_logic_vector(3 downto 4); yg : buffer time_vector(3 to 4));
end paycknoacn;

library ieee;
use ieee.std_logic_1164.all;

architecture eqcxe of paycknoacn is
  signal hhp : std_logic_vector(0 to 3);
  signal nxofwgczho : std_logic;
  signal qipca : std_logic_vector(0 to 3);
  signal yr : std_logic;
  signal le : std_logic_vector(0 to 3);
  signal weae : std_logic;
begin
  riwbouo : entity work.vjas
    port map (celkh => weae, mpfui => le);
  sspvnxa : entity work.vjas
    port map (celkh => yr, mpfui => qipca);
  l : entity work.vjas
    port map (celkh => nxofwgczho, mpfui => le);
  ytoei : entity work.vjas
    port map (celkh => nxofwgczho, mpfui => hhp);
  
  -- Single-driven assignments
  yg <= (8#2_6_4.1_6_7_5_4# us, 411 fs);
end eqcxe;

entity j is
  port (d : out string(1 to 4));
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture rhlffo of j is
  signal hsmvxi : time_vector(3 to 4);
  signal eagdv : std_logic_vector(3 downto 4);
  signal lzsosxs : boolean;
begin
  xbvbgjgb : entity work.paycknoacn
    port map (kndzbye => lzsosxs, khjhaoyn => eagdv, yg => hsmvxi);
  
  -- Multi-driven assignments
  eagdv <= "";
  eagdv <= "";
end rhlffo;



-- Seed after: 3277736464091583758,4404421571376382767
