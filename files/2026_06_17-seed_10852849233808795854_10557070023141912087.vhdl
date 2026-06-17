-- Seed: 10852849233808795854,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity evkclyb is
  port (idtie : inout time; nqm : linkage real; licgbjqu : in bit_vector(2 to 4); mhmacaiqs : buffer std_logic_vector(1 downto 4));
end evkclyb;

architecture xefcpbozq of evkclyb is
  
begin
  -- Multi-driven assignments
  mhmacaiqs <= (others => '0');
end xefcpbozq;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (yzrclilpav : linkage std_logic_vector(1 to 0); duxv : linkage integer; g : out integer_vector(1 to 0));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture mtdaa of f is
  signal obvvaap : std_logic_vector(1 downto 4);
  signal njsrnypi : real;
  signal ptqxbwdwq : time;
  signal akorqkmw : std_logic_vector(1 downto 4);
  signal vujktebwd : bit_vector(2 to 4);
  signal iecu : real;
  signal jmtvsgde : time;
  signal keuku : std_logic_vector(1 downto 4);
  signal htq : bit_vector(2 to 4);
  signal fdulnlf : real;
  signal khnuuv : time;
begin
  smsjpbzwq : entity work.evkclyb
    port map (idtie => khnuuv, nqm => fdulnlf, licgbjqu => htq, mhmacaiqs => keuku);
  ijbd : entity work.evkclyb
    port map (idtie => jmtvsgde, nqm => iecu, licgbjqu => vujktebwd, mhmacaiqs => akorqkmw);
  mnwidgoniu : entity work.evkclyb
    port map (idtie => ptqxbwdwq, nqm => njsrnypi, licgbjqu => htq, mhmacaiqs => obvvaap);
  
  -- Single-driven assignments
  vujktebwd <= ('1', '0', '1');
  htq <= ('0', '1', '0');
  g <= (others => 0);
end mtdaa;

entity gkxwlo is
  port (ypu : buffer time);
end gkxwlo;

architecture uyzpvrd of gkxwlo is
  
begin
  -- Single-driven assignments
  ypu <= 2#1# ms;
end uyzpvrd;



-- Seed after: 12118152471214076269,10557070023141912087
