-- Seed: 15647108236442931616,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity yzwwcyuf is
  port (ldtz : in std_logic_vector(3 downto 1); mxao : inout std_logic);
end yzwwcyuf;

architecture arevt of yzwwcyuf is
  
begin
  -- Multi-driven assignments
  mxao <= mxao;
end arevt;

library ieee;
use ieee.std_logic_1164.all;

entity eojomcg is
  port (faib : out std_logic; ec : buffer integer);
end eojomcg;

architecture amfmoyr of eojomcg is
  
begin
  -- Multi-driven assignments
  faib <= '0';
  faib <= faib;
end amfmoyr;

entity k is
  port (ebmb : in integer_vector(2 downto 3));
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture pfzamavhvd of k is
  signal ojfyue : integer;
  signal gkrh : std_logic_vector(3 downto 1);
  signal tidct : std_logic;
  signal gsnuimtmm : std_logic_vector(3 downto 1);
begin
  ivp : entity work.yzwwcyuf
    port map (ldtz => gsnuimtmm, mxao => tidct);
  yzacucx : entity work.yzwwcyuf
    port map (ldtz => gkrh, mxao => tidct);
  booje : entity work.eojomcg
    port map (faib => tidct, ec => ojfyue);
end pfzamavhvd;



-- Seed after: 18348441383133629437,5444032936641997477
