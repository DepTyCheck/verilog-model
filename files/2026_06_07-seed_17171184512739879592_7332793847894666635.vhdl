-- Seed: 17171184512739879592,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity dmiaq is
  port (fptnemrs : inout std_logic_vector(2 to 0));
end dmiaq;



architecture eyxcqmzgff of dmiaq is
  
begin
  
end eyxcqmzgff;



entity tkexko is
  port (goakrm : inout time_vector(2 downto 4); u : in time; goxqog : out integer; vfjly : buffer integer);
end tkexko;

library ieee;
use ieee.std_logic_1164.all;

architecture slbvdxgbp of tkexko is
  signal vomby : std_logic_vector(2 to 0);
begin
  prftpv : entity work.dmiaq
    port map (fptnemrs => vomby);
end slbvdxgbp;



entity io is
  port (piw : linkage integer; iqgelvqx : buffer time; feqbbeip : buffer real_vector(4 downto 4));
end io;

library ieee;
use ieee.std_logic_1164.all;

architecture o of io is
  signal vnflptrb : std_logic_vector(2 to 0);
  signal dctwpj : std_logic_vector(2 to 0);
  signal sqnfradssw : std_logic_vector(2 to 0);
  signal uksjtvyl : integer;
  signal rkgxfddnn : integer;
  signal umbfnhbeh : time_vector(2 downto 4);
begin
  fqye : entity work.tkexko
    port map (goakrm => umbfnhbeh, u => iqgelvqx, goxqog => rkgxfddnn, vfjly => uksjtvyl);
  p : entity work.dmiaq
    port map (fptnemrs => sqnfradssw);
  mpjcebco : entity work.dmiaq
    port map (fptnemrs => dctwpj);
  ribtx : entity work.dmiaq
    port map (fptnemrs => vnflptrb);
end o;

library ieee;
use ieee.std_logic_1164.all;

entity leny is
  port (zqmafwm : out real; usdp : out std_logic_vector(2 to 1); ttks : out std_logic; kxbcqegv : out integer_vector(1 downto 1));
end leny;



architecture rpgd of leny is
  
begin
  
end rpgd;



-- Seed after: 9759218974634135843,7332793847894666635
