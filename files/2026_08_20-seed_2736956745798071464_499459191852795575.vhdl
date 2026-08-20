-- Seed: 2736956745798071464,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity ypquus is
  port (q : buffer time_vector(3 to 2); otawbdwd : inout std_logic_vector(3 downto 2));
end ypquus;

architecture c of ypquus is
  
begin
  -- Single-driven assignments
  q <= q;
  
  -- Multi-driven assignments
  otawbdwd <= ('H', 'H');
  otawbdwd <= ('-', '-');
  otawbdwd <= otawbdwd;
  otawbdwd <= ('H', '-');
end c;

library ieee;
use ieee.std_logic_1164.all;

entity kuesxdm is
  port (z : linkage std_logic; gjynvi : out std_logic);
end kuesxdm;

library ieee;
use ieee.std_logic_1164.all;

architecture s of kuesxdm is
  signal txxxv : time_vector(3 to 2);
  signal gadoaeew : std_logic_vector(3 downto 2);
  signal rpd : time_vector(3 to 2);
  signal bpm : time_vector(3 to 2);
  signal vpbzu : std_logic_vector(3 downto 2);
  signal lucudxbxj : time_vector(3 to 2);
begin
  kiljlsjkg : entity work.ypquus
    port map (q => lucudxbxj, otawbdwd => vpbzu);
  v : entity work.ypquus
    port map (q => bpm, otawbdwd => vpbzu);
  lukghuxx : entity work.ypquus
    port map (q => rpd, otawbdwd => gadoaeew);
  j : entity work.ypquus
    port map (q => txxxv, otawbdwd => vpbzu);
  
  -- Multi-driven assignments
  gjynvi <= gjynvi;
  gjynvi <= gjynvi;
  gadoaeew <= "ZW";
  gadoaeew <= "U0";
end s;



-- Seed after: 7126200844385372142,499459191852795575
