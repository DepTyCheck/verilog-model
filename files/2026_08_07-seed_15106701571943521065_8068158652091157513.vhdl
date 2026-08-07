-- Seed: 15106701571943521065,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity ehjfzagyx is
  port (p : buffer std_logic; ke : linkage std_logic_vector(0 to 2));
end ehjfzagyx;

architecture bzvymsdir of ehjfzagyx is
  
begin
  -- Multi-driven assignments
  p <= 'Z';
  p <= p;
  p <= p;
  p <= p;
end bzvymsdir;

library ieee;
use ieee.std_logic_1164.all;

entity kg is
  port (nwqe : buffer real_vector(0 to 3); qddrwign : in std_logic; kulncbbxaj : inout integer; iyqjehg : linkage std_logic);
end kg;

library ieee;
use ieee.std_logic_1164.all;

architecture pf of kg is
  signal etr : std_logic_vector(0 to 2);
  signal ablx : std_logic;
begin
  gxmxl : entity work.ehjfzagyx
    port map (p => ablx, ke => etr);
end pf;

library ieee;
use ieee.std_logic_1164.all;

entity aquhxlxq is
  port (jmmkjqqs : in std_logic);
end aquhxlxq;

library ieee;
use ieee.std_logic_1164.all;

architecture twbrex of aquhxlxq is
  signal yuhgks : std_logic;
  signal drd : integer;
  signal egiq : real_vector(0 to 3);
  signal qyaqkkpy : std_logic_vector(0 to 2);
  signal qmoekpstb : std_logic;
begin
  ujfwpbxu : entity work.ehjfzagyx
    port map (p => qmoekpstb, ke => qyaqkkpy);
  g : entity work.kg
    port map (nwqe => egiq, qddrwign => jmmkjqqs, kulncbbxaj => drd, iyqjehg => yuhgks);
  
  -- Multi-driven assignments
  qmoekpstb <= '0';
end twbrex;

library ieee;
use ieee.std_logic_1164.all;

entity pseoukvben is
  port (lkwmhlbztx : in std_logic);
end pseoukvben;

library ieee;
use ieee.std_logic_1164.all;

architecture ygzvdrx of pseoukvben is
  signal rohyvlermb : std_logic;
  signal dqlvgyj : std_logic;
  signal ittqpahpc : std_logic;
  signal nugo : std_logic_vector(0 to 2);
  signal l : std_logic;
begin
  eazqki : entity work.ehjfzagyx
    port map (p => l, ke => nugo);
  ekgulayus : entity work.ehjfzagyx
    port map (p => ittqpahpc, ke => nugo);
  ewsflurl : entity work.ehjfzagyx
    port map (p => dqlvgyj, ke => nugo);
  elmbuqrdf : entity work.aquhxlxq
    port map (jmmkjqqs => rohyvlermb);
end ygzvdrx;



-- Seed after: 4582916820803991812,8068158652091157513
