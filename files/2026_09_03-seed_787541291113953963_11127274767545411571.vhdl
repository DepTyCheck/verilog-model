-- Seed: 787541291113953963,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity pdnynsip is
  port (nvszvapwzg : buffer bit; ifxu : inout std_logic_vector(1 to 0); rvu : in std_logic_vector(1 downto 4); fchgxpzgml : buffer severity_level);
end pdnynsip;

architecture rujw of pdnynsip is
  
begin
  -- Single-driven assignments
  fchgxpzgml <= NOTE;
  nvszvapwzg <= '0';
end rujw;

library ieee;
use ieee.std_logic_1164.all;

entity mqqn is
  port (frc : buffer std_logic);
end mqqn;

library ieee;
use ieee.std_logic_1164.all;

architecture iattfukh of mqqn is
  signal iubmgpynny : severity_level;
  signal mzwa : std_logic_vector(1 downto 4);
  signal ibfrh : std_logic_vector(1 to 0);
  signal pstipl : bit;
  signal avli : severity_level;
  signal gdtqdm : std_logic_vector(1 downto 4);
  signal yrn : bit;
begin
  ga : entity work.pdnynsip
    port map (nvszvapwzg => yrn, ifxu => gdtqdm, rvu => gdtqdm, fchgxpzgml => avli);
  hciwtw : entity work.pdnynsip
    port map (nvszvapwzg => pstipl, ifxu => ibfrh, rvu => mzwa, fchgxpzgml => iubmgpynny);
  
  -- Multi-driven assignments
  mzwa <= mzwa;
end iattfukh;



-- Seed after: 15537444680135096332,11127274767545411571
