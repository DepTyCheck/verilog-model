-- Seed: 4620880298545677777,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity eclwd is
  port (tvosqwii : inout boolean; jwkovsbzd : inout std_logic_vector(3 downto 4); pveztvdkl : out real_vector(3 downto 0));
end eclwd;



architecture sjhsvdaira of eclwd is
  
begin
  
end sjhsvdaira;



entity qmgvfp is
  port (p : out real);
end qmgvfp;

library ieee;
use ieee.std_logic_1164.all;

architecture fsd of qmgvfp is
  signal glocifh : real_vector(3 downto 0);
  signal vigl : boolean;
  signal zgkb : real_vector(3 downto 0);
  signal fl : boolean;
  signal foyadzf : real_vector(3 downto 0);
  signal rnryto : std_logic_vector(3 downto 4);
  signal rrx : boolean;
begin
  pr : entity work.eclwd
    port map (tvosqwii => rrx, jwkovsbzd => rnryto, pveztvdkl => foyadzf);
  dqdckiww : entity work.eclwd
    port map (tvosqwii => fl, jwkovsbzd => rnryto, pveztvdkl => zgkb);
  dlrvr : entity work.eclwd
    port map (tvosqwii => vigl, jwkovsbzd => rnryto, pveztvdkl => glocifh);
end fsd;



-- Seed after: 6294056712937833111,6298042963991371649
