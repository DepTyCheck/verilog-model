-- Seed: 1943943259894329524,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity bbk is
  port (deq : in std_logic_vector(2 downto 0); t : out integer; apelzxih : buffer time; jgj : buffer std_logic_vector(0 to 1));
end bbk;

architecture cbjzpqr of bbk is
  
begin
  -- Single-driven assignments
  t <= 2#1110#;
  apelzxih <= 2_2_4_4_1 ms;
  
  -- Multi-driven assignments
  jgj <= ('L', 'Z');
end cbjzpqr;

entity vnplwgupfc is
  port (ca : in real);
end vnplwgupfc;

library ieee;
use ieee.std_logic_1164.all;

architecture hyjpzqj of vnplwgupfc is
  signal ixduhtgkr : std_logic_vector(0 to 1);
  signal mxpmwbu : time;
  signal cxqzwfngu : integer;
  signal rjgicmpv : std_logic_vector(0 to 1);
  signal qpvvlqls : time;
  signal krzqabeblz : integer;
  signal yuth : time;
  signal jgmrbhrhm : integer;
  signal lwqma : std_logic_vector(2 downto 0);
  signal gck : std_logic_vector(0 to 1);
  signal t : time;
  signal issob : integer;
  signal xloomf : std_logic_vector(2 downto 0);
begin
  qwouz : entity work.bbk
    port map (deq => xloomf, t => issob, apelzxih => t, jgj => gck);
  vuquucv : entity work.bbk
    port map (deq => lwqma, t => jgmrbhrhm, apelzxih => yuth, jgj => gck);
  uthqsekovm : entity work.bbk
    port map (deq => lwqma, t => krzqabeblz, apelzxih => qpvvlqls, jgj => rjgicmpv);
  rjgvnfnof : entity work.bbk
    port map (deq => lwqma, t => cxqzwfngu, apelzxih => mxpmwbu, jgj => ixduhtgkr);
  
  -- Multi-driven assignments
  xloomf <= ('H', 'W', 'W');
  gck <= "ZU";
  gck <= ('0', 'W');
  lwqma <= "UUL";
end hyjpzqj;

library ieee;
use ieee.std_logic_1164.all;

entity cg is
  port (wcesqqhwj : linkage time; wdcuguqz : out real; lcty : inout std_logic);
end cg;

library ieee;
use ieee.std_logic_1164.all;

architecture yogytj of cg is
  signal melvyxcikt : std_logic_vector(0 to 1);
  signal epoejcg : time;
  signal rh : integer;
  signal mg : std_logic_vector(2 downto 0);
  signal vdlhiy : time;
  signal hwocvzwqs : integer;
  signal xl : std_logic_vector(2 downto 0);
  signal btgsimqpi : std_logic_vector(0 to 1);
  signal yb : time;
  signal mebp : integer;
  signal bwplm : std_logic_vector(2 downto 0);
begin
  xucynjpda : entity work.bbk
    port map (deq => bwplm, t => mebp, apelzxih => yb, jgj => btgsimqpi);
  ywuhbe : entity work.vnplwgupfc
    port map (ca => wdcuguqz);
  peiobpt : entity work.bbk
    port map (deq => xl, t => hwocvzwqs, apelzxih => vdlhiy, jgj => btgsimqpi);
  fumx : entity work.bbk
    port map (deq => mg, t => rh, apelzxih => epoejcg, jgj => melvyxcikt);
  
  -- Single-driven assignments
  wdcuguqz <= 16#F.F8BF#;
  
  -- Multi-driven assignments
  bwplm <= ('X', 'Z', '1');
  lcty <= 'W';
end yogytj;



-- Seed after: 16546859954645564530,8118127366649987907
