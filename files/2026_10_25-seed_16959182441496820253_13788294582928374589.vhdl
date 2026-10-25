-- Seed: 16959182441496820253,13788294582928374589

entity dxmevrnx is
  port (qyhbtqucl : inout integer_vector(1 downto 1));
end dxmevrnx;

architecture ifzpoue of dxmevrnx is
  
begin
  -- Single-driven assignments
  qyhbtqucl <= (others => 0_2_4);
end ifzpoue;

entity xijbajlv is
  port (sfdrhlqmec : buffer string(5 downto 2); cyp : inout time; gfltjosile : buffer character);
end xijbajlv;

architecture yvcqjtyl of xijbajlv is
  signal xgnvdtox : integer_vector(1 downto 1);
  signal j : integer_vector(1 downto 1);
  signal jjaxgfq : integer_vector(1 downto 1);
begin
  wyiepdih : entity work.dxmevrnx
    port map (qyhbtqucl => jjaxgfq);
  nrdfmtrs : entity work.dxmevrnx
    port map (qyhbtqucl => j);
  lcspksf : entity work.dxmevrnx
    port map (qyhbtqucl => xgnvdtox);
  
  -- Single-driven assignments
  cyp <= cyp;
  gfltjosile <= 'y';
  sfdrhlqmec <= "jcgz";
end yvcqjtyl;

library ieee;
use ieee.std_logic_1164.all;

entity twjujegakf is
  port (w : out std_logic_vector(2 to 1));
end twjujegakf;

architecture wlyzmzbze of twjujegakf is
  signal rofru : character;
  signal ymlqoxjfc : time;
  signal nulrbdr : string(5 downto 2);
  signal buqo : character;
  signal mcywqomwru : time;
  signal doi : string(5 downto 2);
begin
  nrswrsepm : entity work.xijbajlv
    port map (sfdrhlqmec => doi, cyp => mcywqomwru, gfltjosile => buqo);
  zkzscn : entity work.xijbajlv
    port map (sfdrhlqmec => nulrbdr, cyp => ymlqoxjfc, gfltjosile => rofru);
  
  -- Multi-driven assignments
  w <= (others => '0');
  w <= "";
end wlyzmzbze;



-- Seed after: 1866097360655898484,13788294582928374589
