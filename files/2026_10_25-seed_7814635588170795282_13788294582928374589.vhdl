-- Seed: 7814635588170795282,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity oee is
  port (cvpidddcx : in std_logic; zrdqdff : linkage std_logic_vector(0 downto 0));
end oee;

architecture wehhflfb of oee is
  
begin
  
end wehhflfb;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (ydrxe : linkage std_logic; nsop : linkage boolean_vector(0 to 2); hbmau : in integer);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture na of r is
  signal cfalmtgsi : std_logic_vector(0 downto 0);
  signal gfkvapl : std_logic;
  signal zgyrgju : std_logic;
  signal ovymx : std_logic_vector(0 downto 0);
  signal y : std_logic;
begin
  irdp : entity work.oee
    port map (cvpidddcx => y, zrdqdff => ovymx);
  cdeo : entity work.oee
    port map (cvpidddcx => zgyrgju, zrdqdff => ovymx);
  bkns : entity work.oee
    port map (cvpidddcx => gfkvapl, zrdqdff => cfalmtgsi);
  
  -- Multi-driven assignments
  y <= y;
  cfalmtgsi <= (others => 'X');
  y <= '1';
  y <= y;
end na;

library ieee;
use ieee.std_logic_1164.all;

entity ppdnvvt is
  port (epq : linkage std_logic; irp : buffer integer);
end ppdnvvt;

library ieee;
use ieee.std_logic_1164.all;

architecture pdy of ppdnvvt is
  signal zndrn : std_logic;
  signal ysrbdf : std_logic_vector(0 downto 0);
  signal hjo : std_logic;
begin
  it : entity work.oee
    port map (cvpidddcx => hjo, zrdqdff => ysrbdf);
  zauqsjqtlu : entity work.oee
    port map (cvpidddcx => zndrn, zrdqdff => ysrbdf);
  
  -- Multi-driven assignments
  zndrn <= hjo;
  hjo <= 'U';
  hjo <= '0';
end pdy;



-- Seed after: 10215127389365907396,13788294582928374589
