-- Seed: 4272708648922171580,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity ndsajhr is
  port (kdtvesg : buffer std_logic; xphee : in time; igqiq : linkage boolean_vector(3 downto 1); yqjj : inout time);
end ndsajhr;

architecture lwqr of ndsajhr is
  
begin
  -- Single-driven assignments
  yqjj <= 0 ns;
end lwqr;

library ieee;
use ieee.std_logic_1164.all;

entity hksraunti is
  port (cigqytmb : linkage std_logic_vector(3 to 4));
end hksraunti;

library ieee;
use ieee.std_logic_1164.all;

architecture naumlkyz of hksraunti is
  signal it : boolean_vector(3 downto 1);
  signal erg : time;
  signal guqeb : boolean_vector(3 downto 1);
  signal jgjjbvbw : time;
  signal rnp : std_logic;
begin
  jpsjv : entity work.ndsajhr
    port map (kdtvesg => rnp, xphee => jgjjbvbw, igqiq => guqeb, yqjj => jgjjbvbw);
  hs : entity work.ndsajhr
    port map (kdtvesg => rnp, xphee => erg, igqiq => it, yqjj => erg);
  
  -- Multi-driven assignments
  rnp <= 'U';
end naumlkyz;



-- Seed after: 9046802659520222888,2511821214772927453
