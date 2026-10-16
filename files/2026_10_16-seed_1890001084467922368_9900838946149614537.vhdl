-- Seed: 1890001084467922368,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity awyao is
  port (mrdqz : in real; yvuokcrsea : buffer time_vector(2 to 4); ezz : inout std_logic; iaz : in integer);
end awyao;

architecture rxmc of awyao is
  
begin
  -- Single-driven assignments
  yvuokcrsea <= (0.04220 ps, 20.3_1_1_1_0 ms, 1 hr);
end rxmc;

library ieee;
use ieee.std_logic_1164.all;

entity azh is
  port (ix : in time; gjepfuslxw : in bit_vector(4 downto 1); cwsmugk : out std_logic; umj : linkage time);
end azh;

library ieee;
use ieee.std_logic_1164.all;

architecture fgvexohkpn of azh is
  signal xfgpc : integer;
  signal nkhmygglr : std_logic;
  signal qqwnf : time_vector(2 to 4);
  signal yidla : time_vector(2 to 4);
  signal ladvky : integer;
  signal betqwmv : time_vector(2 to 4);
  signal ucikpf : real;
begin
  bif : entity work.awyao
    port map (mrdqz => ucikpf, yvuokcrsea => betqwmv, ezz => cwsmugk, iaz => ladvky);
  dtwnfz : entity work.awyao
    port map (mrdqz => ucikpf, yvuokcrsea => yidla, ezz => cwsmugk, iaz => ladvky);
  awgo : entity work.awyao
    port map (mrdqz => ucikpf, yvuokcrsea => qqwnf, ezz => nkhmygglr, iaz => xfgpc);
  
  -- Single-driven assignments
  ucikpf <= 2#1_0_1_0.1_1#;
  
  -- Multi-driven assignments
  cwsmugk <= cwsmugk;
  cwsmugk <= 'Z';
end fgvexohkpn;



-- Seed after: 12622164705621137920,9900838946149614537
