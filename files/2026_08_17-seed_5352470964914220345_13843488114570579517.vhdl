-- Seed: 5352470964914220345,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity jbuqil is
  port (eynjb : inout std_logic_vector(2 downto 1); n : in std_logic);
end jbuqil;

architecture afbmhwcjw of jbuqil is
  
begin
  
end afbmhwcjw;

library ieee;
use ieee.std_logic_1164.all;

entity tmuvpzorff is
  port (e : inout integer; qfyikaifs : buffer std_logic_vector(0 to 1));
end tmuvpzorff;

library ieee;
use ieee.std_logic_1164.all;

architecture vsrwirkphs of tmuvpzorff is
  signal o : std_logic_vector(2 downto 1);
  signal tdsdkopo : std_logic;
begin
  yvlllt : entity work.jbuqil
    port map (eynjb => qfyikaifs, n => tdsdkopo);
  ksgmiqnc : entity work.jbuqil
    port map (eynjb => o, n => tdsdkopo);
  
  -- Single-driven assignments
  e <= 2;
  
  -- Multi-driven assignments
  o <= qfyikaifs;
  tdsdkopo <= 'L';
end vsrwirkphs;



-- Seed after: 12213208462692857908,13843488114570579517
