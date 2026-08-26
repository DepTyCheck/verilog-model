-- Seed: 2659017335548773573,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity pekp is
  port (vokqo : inout std_logic_vector(2 to 4); ppidgsx : buffer std_logic; rh : out time);
end pekp;

architecture cg of pekp is
  
begin
  -- Single-driven assignments
  rh <= rh;
  
  -- Multi-driven assignments
  vokqo <= "ZU0";
  ppidgsx <= ppidgsx;
  vokqo <= "XUZ";
  ppidgsx <= ppidgsx;
end cg;

library ieee;
use ieee.std_logic_1164.all;

entity ksmcxvjmo is
  port (m : buffer time; mnydrvjnwf : out std_logic);
end ksmcxvjmo;

library ieee;
use ieee.std_logic_1164.all;

architecture c of ksmcxvjmo is
  signal acopdiq : time;
  signal y : std_logic_vector(2 to 4);
begin
  gqqnssx : entity work.pekp
    port map (vokqo => y, ppidgsx => mnydrvjnwf, rh => acopdiq);
  
  -- Single-driven assignments
  m <= m;
  
  -- Multi-driven assignments
  mnydrvjnwf <= 'U';
  mnydrvjnwf <= mnydrvjnwf;
  mnydrvjnwf <= 'X';
end c;

entity gkes is
  port (li : inout real_vector(4 to 0); m : in boolean);
end gkes;

library ieee;
use ieee.std_logic_1164.all;

architecture f of gkes is
  signal zbbqivcze : time;
  signal quyajroht : std_logic_vector(2 to 4);
  signal yi : time;
  signal xlwnojp : std_logic_vector(2 to 4);
  signal rmtiuybz : time;
  signal htjv : std_logic_vector(2 to 4);
  signal uthgacfny : time;
  signal ilsowtqjco : std_logic;
  signal nphpaac : std_logic_vector(2 to 4);
begin
  ex : entity work.pekp
    port map (vokqo => nphpaac, ppidgsx => ilsowtqjco, rh => uthgacfny);
  kwcldlihcw : entity work.pekp
    port map (vokqo => htjv, ppidgsx => ilsowtqjco, rh => rmtiuybz);
  vswngl : entity work.pekp
    port map (vokqo => xlwnojp, ppidgsx => ilsowtqjco, rh => yi);
  sbnbpvxkzf : entity work.pekp
    port map (vokqo => quyajroht, ppidgsx => ilsowtqjco, rh => zbbqivcze);
  
  -- Single-driven assignments
  li <= (others => 0.0);
  
  -- Multi-driven assignments
  nphpaac <= ('L', 'H', 'U');
end f;

entity dqq is
  port (ffwtv : inout integer; gvqxpmvh : in integer; vmdn : in severity_level);
end dqq;

architecture kz of dqq is
  signal pu : boolean;
  signal mkk : real_vector(4 to 0);
begin
  djgjx : entity work.gkes
    port map (li => mkk, m => pu);
  
  -- Single-driven assignments
  ffwtv <= gvqxpmvh;
  pu <= pu;
end kz;



-- Seed after: 7907217619208367266,6000118208082478503
