-- Seed: 6233307619372534927,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity dpvlw is
  port (mvvny : out real; mwdp : out real; tz : in time; dxwyaw : linkage std_logic);
end dpvlw;

architecture tpsca of dpvlw is
  
begin
  -- Single-driven assignments
  mwdp <= mwdp;
  mvvny <= mwdp;
end tpsca;

library ieee;
use ieee.std_logic_1164.all;

entity ld is
  port (rnz : out std_logic_vector(4 downto 1); odcikiqd : inout boolean_vector(2 downto 0));
end ld;

library ieee;
use ieee.std_logic_1164.all;

architecture w of ld is
  signal cnyd : std_logic;
  signal hxaqur : time;
  signal f : real;
  signal fqkxjwyl : real;
begin
  wnaevyejm : entity work.dpvlw
    port map (mvvny => fqkxjwyl, mwdp => f, tz => hxaqur, dxwyaw => cnyd);
  
  -- Single-driven assignments
  odcikiqd <= (TRUE, FALSE, FALSE);
  hxaqur <= 8#315.0_1_6# ms;
  
  -- Multi-driven assignments
  cnyd <= 'H';
  rnz <= ('1', 'L', '-', 'U');
  rnz <= "UHXZ";
  rnz <= rnz;
end w;

library ieee;
use ieee.std_logic_1164.all;

entity nsdtoiczx is
  port (mds : buffer real_vector(3 downto 0); elotwuv : linkage std_logic; iadyukdid : buffer std_logic_vector(3 to 2); fj : out real);
end nsdtoiczx;

library ieee;
use ieee.std_logic_1164.all;

architecture mowgs of nsdtoiczx is
  signal bdce : time;
  signal elbagnva : real;
  signal kmogrenzd : std_logic;
  signal aczoh : time;
  signal p : real;
  signal yppww : real;
begin
  wfuexyw : entity work.dpvlw
    port map (mvvny => yppww, mwdp => p, tz => aczoh, dxwyaw => kmogrenzd);
  w : entity work.dpvlw
    port map (mvvny => fj, mwdp => elbagnva, tz => bdce, dxwyaw => elotwuv);
  
  -- Single-driven assignments
  mds <= mds;
  aczoh <= aczoh;
  bdce <= aczoh;
  
  -- Multi-driven assignments
  iadyukdid <= (others => '0');
end mowgs;



-- Seed after: 8029201429678575665,13592003931158285879
