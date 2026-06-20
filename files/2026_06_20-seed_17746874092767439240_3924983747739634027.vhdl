-- Seed: 17746874092767439240,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity krzuoqy is
  port (rjn : inout std_logic; cuwbhyoui : buffer std_logic_vector(1 downto 0));
end krzuoqy;

architecture jmun of krzuoqy is
  
begin
  -- Multi-driven assignments
  cuwbhyoui <= ('0', 'X');
  cuwbhyoui <= ('1', '1');
  cuwbhyoui <= ('0', '0');
end jmun;

library ieee;
use ieee.std_logic_1164.all;

entity jopyryj is
  port (nfzfog : buffer std_logic_vector(1 to 2));
end jopyryj;

library ieee;
use ieee.std_logic_1164.all;

architecture sdgea of jopyryj is
  signal deflh : std_logic_vector(1 downto 0);
  signal xxaadd : std_logic;
begin
  naowhixsw : entity work.krzuoqy
    port map (rjn => xxaadd, cuwbhyoui => deflh);
  
  -- Multi-driven assignments
  deflh <= ('U', '-');
  deflh <= "X0";
  nfzfog <= ('L', 'Z');
  nfzfog <= ('H', 'U');
end sdgea;

library ieee;
use ieee.std_logic_1164.all;

entity mv is
  port (zd : in std_logic; irwzfzed : in std_logic_vector(4 to 1); cij : buffer std_logic_vector(0 downto 1); jropll : out time);
end mv;

library ieee;
use ieee.std_logic_1164.all;

architecture kh of mv is
  signal o : std_logic_vector(1 downto 0);
  signal fxd : std_logic;
  signal pdfaah : std_logic_vector(1 downto 0);
begin
  yaxgryj : entity work.jopyryj
    port map (nfzfog => pdfaah);
  bdqija : entity work.krzuoqy
    port map (rjn => fxd, cuwbhyoui => pdfaah);
  vnfbsklxx : entity work.krzuoqy
    port map (rjn => fxd, cuwbhyoui => o);
end kh;



-- Seed after: 7802456554596334142,3924983747739634027
