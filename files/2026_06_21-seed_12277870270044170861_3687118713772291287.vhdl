-- Seed: 12277870270044170861,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity cfcgxa is
  port (bvvvwiks : linkage boolean; gasxop : buffer std_logic_vector(4 downto 3));
end cfcgxa;

architecture drvwy of cfcgxa is
  
begin
  -- Multi-driven assignments
  gasxop <= "L1";
  gasxop <= "UL";
  gasxop <= ('L', '1');
  gasxop <= ('W', 'W');
end drvwy;

library ieee;
use ieee.std_logic_1164.all;

entity zzxc is
  port (aocfx : in std_logic);
end zzxc;

library ieee;
use ieee.std_logic_1164.all;

architecture mbkdpw of zzxc is
  signal zaabelye : std_logic_vector(4 downto 3);
  signal aucpjhbto : boolean;
begin
  vxbdmpmv : entity work.cfcgxa
    port map (bvvvwiks => aucpjhbto, gasxop => zaabelye);
  
  -- Multi-driven assignments
  zaabelye <= ('0', '-');
end mbkdpw;

library ieee;
use ieee.std_logic_1164.all;

entity chi is
  port (agsayjvwkk : in std_logic; fncto : in std_logic_vector(0 to 2); ryzkvththf : linkage std_logic_vector(1 to 4); niqxgziu : inout bit);
end chi;

architecture jissz of chi is
  
begin
  hvmpzplwyg : entity work.zzxc
    port map (aocfx => agsayjvwkk);
  
  -- Single-driven assignments
  niqxgziu <= '1';
end jissz;



-- Seed after: 7613260852593171579,3687118713772291287
