-- Seed: 5042047182525154649,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity feppqleba is
  port (oqx : buffer real_vector(0 downto 1); rgr : linkage std_logic_vector(2 to 2));
end feppqleba;



architecture rwaznvngm of feppqleba is
  
begin
  
end rwaznvngm;

library ieee;
use ieee.std_logic_1164.all;

entity dwmlgy is
  port (vp : buffer std_logic; doxcjvd : linkage std_logic_vector(1 to 2); uq : out real);
end dwmlgy;

library ieee;
use ieee.std_logic_1164.all;

architecture umqjxrkikr of dwmlgy is
  signal gxsn : real_vector(0 downto 1);
  signal e : std_logic_vector(2 to 2);
  signal f : real_vector(0 downto 1);
begin
  flk : entity work.feppqleba
    port map (oqx => f, rgr => e);
  b : entity work.feppqleba
    port map (oqx => gxsn, rgr => e);
end umqjxrkikr;

library ieee;
use ieee.std_logic_1164.all;

entity snpwycuj is
  port (qmqre : inout integer; btoe : buffer integer; rdtadstsc : in std_logic_vector(0 downto 1); sd : inout integer);
end snpwycuj;

library ieee;
use ieee.std_logic_1164.all;

architecture x of snpwycuj is
  signal qtae : real_vector(0 downto 1);
  signal byjsqhett : std_logic_vector(2 to 2);
  signal uberrgpr : real_vector(0 downto 1);
  signal talxfisrj : std_logic_vector(2 to 2);
  signal mlv : real_vector(0 downto 1);
begin
  ualsixag : entity work.feppqleba
    port map (oqx => mlv, rgr => talxfisrj);
  aidw : entity work.feppqleba
    port map (oqx => uberrgpr, rgr => byjsqhett);
  gi : entity work.feppqleba
    port map (oqx => qtae, rgr => byjsqhett);
end x;



entity tptawg is
  port (vrelmokw : buffer real; rjq : buffer boolean_vector(3 downto 4); ltwn : linkage integer; bjwl : linkage bit);
end tptawg;

library ieee;
use ieee.std_logic_1164.all;

architecture zfvpoq of tptawg is
  signal jv : std_logic_vector(1 to 2);
  signal ssvk : std_logic;
  signal iktdkxe : std_logic_vector(2 to 2);
  signal wkq : real_vector(0 downto 1);
  signal lw : integer;
  signal vrw : std_logic_vector(0 downto 1);
  signal qupgpzbbe : integer;
  signal tx : integer;
  signal mdsseipas : integer;
  signal odd : std_logic_vector(0 downto 1);
  signal tsgvwzu : integer;
  signal n : integer;
begin
  fziiv : entity work.snpwycuj
    port map (qmqre => n, btoe => tsgvwzu, rdtadstsc => odd, sd => mdsseipas);
  xyffy : entity work.snpwycuj
    port map (qmqre => tx, btoe => qupgpzbbe, rdtadstsc => vrw, sd => lw);
  yevhzvzvgt : entity work.feppqleba
    port map (oqx => wkq, rgr => iktdkxe);
  jzt : entity work.dwmlgy
    port map (vp => ssvk, doxcjvd => jv, uq => vrelmokw);
end zfvpoq;



-- Seed after: 16821603211044226982,11387579217500963635
