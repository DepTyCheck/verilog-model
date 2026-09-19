-- Seed: 17003026252377292581,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity kfyvcofnwy is
  port (bv : buffer std_logic_vector(0 downto 2); yk : linkage std_logic; olgjfltwip : in std_logic);
end kfyvcofnwy;

architecture xjrcnrg of kfyvcofnwy is
  
begin
  -- Multi-driven assignments
  bv <= "";
end xjrcnrg;

library ieee;
use ieee.std_logic_1164.all;

entity fihuhlcwc is
  port (vhjtckd : out std_logic_vector(3 to 0); cnosw : buffer bit);
end fihuhlcwc;

library ieee;
use ieee.std_logic_1164.all;

architecture y of fihuhlcwc is
  signal fbvqolnd : std_logic;
  signal qrqrllg : std_logic;
  signal bccueemzd : std_logic_vector(0 downto 2);
  signal lobncf : std_logic;
  signal bdjukvxd : std_logic_vector(0 downto 2);
begin
  ogstwbgnu : entity work.kfyvcofnwy
    port map (bv => bdjukvxd, yk => lobncf, olgjfltwip => lobncf);
  uaipideh : entity work.kfyvcofnwy
    port map (bv => bccueemzd, yk => qrqrllg, olgjfltwip => fbvqolnd);
  
  -- Single-driven assignments
  cnosw <= '0';
end y;

entity hrdjj is
  port (jnjttsrr : inout bit_vector(3 downto 2); dmxn : in string(2 to 1); yhgcvxsjox : in character);
end hrdjj;

library ieee;
use ieee.std_logic_1164.all;

architecture xhqvgz of hrdjj is
  signal ux : std_logic;
  signal yuhmajgu : std_logic;
  signal ruhpk : std_logic;
  signal tuyodqdas : std_logic_vector(0 downto 2);
  signal cxo : bit;
  signal mcmtprts : std_logic_vector(0 downto 2);
begin
  ncciuq : entity work.fihuhlcwc
    port map (vhjtckd => mcmtprts, cnosw => cxo);
  j : entity work.kfyvcofnwy
    port map (bv => tuyodqdas, yk => ruhpk, olgjfltwip => yuhmajgu);
  e : entity work.kfyvcofnwy
    port map (bv => mcmtprts, yk => ruhpk, olgjfltwip => ruhpk);
  i : entity work.kfyvcofnwy
    port map (bv => mcmtprts, yk => ruhpk, olgjfltwip => ux);
  
  -- Single-driven assignments
  jnjttsrr <= ('1', '0');
  
  -- Multi-driven assignments
  mcmtprts <= "";
  mcmtprts <= (others => '0');
  tuyodqdas <= "";
  tuyodqdas <= (others => '0');
end xhqvgz;



-- Seed after: 5603106072224008448,14141408946471626091
