-- Seed: 10314139274913961190,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity onu is
  port (mpwpoysau : inout std_logic; gtxjllfj : buffer std_logic; qwglfdnoco : linkage bit);
end onu;



architecture hjzo of onu is
  
begin
  
end hjzo;

library ieee;
use ieee.std_logic_1164.all;

entity hyystepkpj is
  port (izdop : in std_logic; arixt : buffer std_logic; pqr : inout time);
end hyystepkpj;



architecture gblr of hyystepkpj is
  
begin
  
end gblr;



entity xxqynbe is
  port (uijim : inout severity_level);
end xxqynbe;

library ieee;
use ieee.std_logic_1164.all;

architecture ysogee of xxqynbe is
  signal vf : bit;
  signal tkffvakrz : std_logic;
  signal i : std_logic;
begin
  gmpcnf : entity work.onu
    port map (mpwpoysau => i, gtxjllfj => tkffvakrz, qwglfdnoco => vf);
end ysogee;



entity nqj is
  port (xidkmvjrgv : inout time; r : buffer integer; uugpoolabo : linkage real);
end nqj;

library ieee;
use ieee.std_logic_1164.all;

architecture cy of nqj is
  signal ta : std_logic;
  signal ybfuqjuzra : std_logic;
  signal aq : std_logic;
  signal kctdujggom : bit;
  signal gz : std_logic;
  signal wf : std_logic;
  signal fkku : std_logic;
begin
  tjugibe : entity work.hyystepkpj
    port map (izdop => fkku, arixt => wf, pqr => xidkmvjrgv);
  az : entity work.onu
    port map (mpwpoysau => wf, gtxjllfj => gz, qwglfdnoco => kctdujggom);
  pqf : entity work.onu
    port map (mpwpoysau => fkku, gtxjllfj => aq, qwglfdnoco => kctdujggom);
  nv : entity work.onu
    port map (mpwpoysau => ybfuqjuzra, gtxjllfj => ta, qwglfdnoco => kctdujggom);
end cy;



-- Seed after: 13212755323100485006,1852660963590380551
