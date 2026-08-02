-- Seed: 3340259224569400640,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity ekdipidr is
  port (tjxbew : in integer; en : inout std_logic_vector(4 to 0); lgaxrfs : in std_logic; ls : inout bit_vector(0 downto 2));
end ekdipidr;

architecture hxhizjz of ekdipidr is
  
begin
  -- Multi-driven assignments
  en <= (others => '0');
  en <= en;
end hxhizjz;

entity lfdskjoad is
  port (csfurvt : in integer; fueam : out time);
end lfdskjoad;

library ieee;
use ieee.std_logic_1164.all;

architecture o of lfdskjoad is
  signal wmdq : bit_vector(0 downto 2);
  signal bghqczbzn : std_logic;
  signal luxykznth : std_logic_vector(4 to 0);
  signal zbt : bit_vector(0 downto 2);
  signal cxmzzk : std_logic;
  signal lvsmon : integer;
  signal jou : bit_vector(0 downto 2);
  signal loewoatvk : std_logic_vector(4 to 0);
  signal qu : integer;
  signal g : bit_vector(0 downto 2);
  signal pvltlacpni : std_logic;
  signal ywredud : std_logic_vector(4 to 0);
begin
  rkwzlzwhx : entity work.ekdipidr
    port map (tjxbew => csfurvt, en => ywredud, lgaxrfs => pvltlacpni, ls => g);
  ieqoppddr : entity work.ekdipidr
    port map (tjxbew => qu, en => loewoatvk, lgaxrfs => pvltlacpni, ls => jou);
  yv : entity work.ekdipidr
    port map (tjxbew => lvsmon, en => ywredud, lgaxrfs => cxmzzk, ls => zbt);
  w : entity work.ekdipidr
    port map (tjxbew => qu, en => luxykznth, lgaxrfs => bghqczbzn, ls => wmdq);
  
  -- Single-driven assignments
  fueam <= fueam;
  lvsmon <= csfurvt;
  qu <= qu;
end o;



-- Seed after: 9183577519014759781,13592003931158285879
