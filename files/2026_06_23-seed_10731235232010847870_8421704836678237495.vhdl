-- Seed: 10731235232010847870,8421704836678237495

entity svff is
  port (bxadmjbge : buffer time);
end svff;

architecture ujhucuz of svff is
  
begin
  -- Single-driven assignments
  bxadmjbge <= 1 ns;
end ujhucuz;

library ieee;
use ieee.std_logic_1164.all;

entity gxc is
  port (corxdg : buffer boolean; yuibowqt : out std_logic_vector(2 downto 2); ggjjh : in std_logic_vector(1 downto 2); nzjnlnwhvz : inout std_logic);
end gxc;

architecture f of gxc is
  signal nxdftkz : time;
begin
  gikr : entity work.svff
    port map (bxadmjbge => nxdftkz);
  
  -- Single-driven assignments
  corxdg <= FALSE;
  
  -- Multi-driven assignments
  nzjnlnwhvz <= 'U';
  yuibowqt <= "U";
  yuibowqt <= "X";
  nzjnlnwhvz <= '-';
end f;

library ieee;
use ieee.std_logic_1164.all;

entity yufe is
  port (dfuzdia : out std_logic_vector(4 to 0); ovtubze : out bit; bye : buffer bit_vector(4 to 2));
end yufe;

library ieee;
use ieee.std_logic_1164.all;

architecture dncphzyun of yufe is
  signal cmbbbh : std_logic;
  signal rngdfbgrv : std_logic_vector(2 downto 2);
  signal xgbr : boolean;
begin
  xwdgrpnoqx : entity work.gxc
    port map (corxdg => xgbr, yuibowqt => rngdfbgrv, ggjjh => dfuzdia, nzjnlnwhvz => cmbbbh);
  
  -- Single-driven assignments
  ovtubze <= '1';
  
  -- Multi-driven assignments
  dfuzdia <= (others => '0');
end dncphzyun;

entity rdx is
  port (yrwqzraabd : buffer integer);
end rdx;

architecture uzkuoqen of rdx is
  
begin
  -- Single-driven assignments
  yrwqzraabd <= 2#1_1_0_1#;
end uzkuoqen;



-- Seed after: 9916997292204062135,8421704836678237495
