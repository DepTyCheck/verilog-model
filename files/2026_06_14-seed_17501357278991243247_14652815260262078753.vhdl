-- Seed: 17501357278991243247,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity xqk is
  port (lsccaw : in boolean_vector(3 downto 0); ricsakddp : buffer boolean; ve : linkage std_logic; ttnzu : buffer bit_vector(0 downto 1));
end xqk;

architecture dxcdxaf of xqk is
  
begin
  -- Single-driven assignments
  ttnzu <= (others => '0');
  ricsakddp <= TRUE;
end dxcdxaf;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (keorud : out std_logic; wwzfz : linkage integer);
end c;

architecture mbxykm of c is
  
begin
  -- Multi-driven assignments
  keorud <= '1';
end mbxykm;

entity mocyu is
  port (tmzberxu : in real; od : buffer integer);
end mocyu;

library ieee;
use ieee.std_logic_1164.all;

architecture zs of mocyu is
  signal ixszpaqcu : bit_vector(0 downto 1);
  signal ukpci : boolean;
  signal auetjqew : boolean_vector(3 downto 0);
  signal kwqm : bit_vector(0 downto 1);
  signal xmdgxaogl : boolean;
  signal oi : boolean_vector(3 downto 0);
  signal cvubottmrg : std_logic;
  signal pqkgzz : integer;
  signal raqq : std_logic;
begin
  jrlfzih : entity work.c
    port map (keorud => raqq, wwzfz => pqkgzz);
  yndju : entity work.c
    port map (keorud => cvubottmrg, wwzfz => od);
  yvktz : entity work.xqk
    port map (lsccaw => oi, ricsakddp => xmdgxaogl, ve => raqq, ttnzu => kwqm);
  bihof : entity work.xqk
    port map (lsccaw => auetjqew, ricsakddp => ukpci, ve => raqq, ttnzu => ixszpaqcu);
  
  -- Multi-driven assignments
  cvubottmrg <= 'U';
  raqq <= 'U';
end zs;



-- Seed after: 5515055064570345361,14652815260262078753
