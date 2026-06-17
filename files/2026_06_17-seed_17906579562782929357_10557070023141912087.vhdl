-- Seed: 17906579562782929357,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity opwa is
  port (vaddiw : inout time; hq : out std_logic_vector(4 to 0); zaurlxie : buffer boolean_vector(4 to 0));
end opwa;

architecture iw of opwa is
  
begin
  -- Single-driven assignments
  zaurlxie <= (others => TRUE);
  
  -- Multi-driven assignments
  hq <= (others => '0');
  hq <= (others => '0');
  hq <= (others => '0');
end iw;

entity ssqkiyhwai is
  port (xaj : in time; ae : buffer bit);
end ssqkiyhwai;

architecture lkvgbk of ssqkiyhwai is
  
begin
  -- Single-driven assignments
  ae <= '1';
end lkvgbk;

entity ndobij is
  port (qjurqhlubg : buffer character; bmgm : linkage character);
end ndobij;

library ieee;
use ieee.std_logic_1164.all;

architecture jlggeb of ndobij is
  signal pjcdahpxq : bit;
  signal jchzeb : boolean_vector(4 to 0);
  signal xvbntggoea : time;
  signal e : boolean_vector(4 to 0);
  signal cq : time;
  signal b : boolean_vector(4 to 0);
  signal njukwaj : std_logic_vector(4 to 0);
  signal byqeqez : time;
begin
  l : entity work.opwa
    port map (vaddiw => byqeqez, hq => njukwaj, zaurlxie => b);
  br : entity work.opwa
    port map (vaddiw => cq, hq => njukwaj, zaurlxie => e);
  ichsw : entity work.opwa
    port map (vaddiw => xvbntggoea, hq => njukwaj, zaurlxie => jchzeb);
  seg : entity work.ssqkiyhwai
    port map (xaj => byqeqez, ae => pjcdahpxq);
  
  -- Single-driven assignments
  qjurqhlubg <= 'y';
  
  -- Multi-driven assignments
  njukwaj <= (others => '0');
  njukwaj <= (others => '0');
  njukwaj <= "";
end jlggeb;

entity dwksji is
  port (zvusmy : out boolean; azqd : buffer integer);
end dwksji;

library ieee;
use ieee.std_logic_1164.all;

architecture nxldl of dwksji is
  signal uafwjft : boolean_vector(4 to 0);
  signal aagxy : std_logic_vector(4 to 0);
  signal jbrish : time;
  signal mo : character;
  signal ssrr : character;
  signal icrd : character;
  signal qfww : character;
begin
  gsu : entity work.ndobij
    port map (qjurqhlubg => qfww, bmgm => icrd);
  itivlgwevn : entity work.ndobij
    port map (qjurqhlubg => ssrr, bmgm => mo);
  kxblnef : entity work.opwa
    port map (vaddiw => jbrish, hq => aagxy, zaurlxie => uafwjft);
  
  -- Single-driven assignments
  azqd <= 3_0;
  zvusmy <= TRUE;
  
  -- Multi-driven assignments
  aagxy <= (others => '0');
  aagxy <= (others => '0');
  aagxy <= "";
end nxldl;



-- Seed after: 6617213866951411895,10557070023141912087
