-- Seed: 9594466497571184252,4404421571376382767

entity lysdmbw is
  port (fnrmtlwx : inout severity_level);
end lysdmbw;

architecture uyxofeqa of lysdmbw is
  
begin
  -- Single-driven assignments
  fnrmtlwx <= NOTE;
end uyxofeqa;

library ieee;
use ieee.std_logic_1164.all;

entity xlxtqzdp is
  port (pwgnvxty : inout std_logic; we : buffer bit_vector(0 downto 3));
end xlxtqzdp;

architecture nljq of xlxtqzdp is
  signal fyml : severity_level;
  signal xap : severity_level;
  signal dqttm : severity_level;
begin
  fqlgthzq : entity work.lysdmbw
    port map (fnrmtlwx => dqttm);
  gkjpsmdju : entity work.lysdmbw
    port map (fnrmtlwx => xap);
  dpbgepo : entity work.lysdmbw
    port map (fnrmtlwx => fyml);
  
  -- Single-driven assignments
  we <= (others => '0');
  
  -- Multi-driven assignments
  pwgnvxty <= 'L';
end nljq;

library ieee;
use ieee.std_logic_1164.all;

entity sckx is
  port (bdvecum : buffer std_logic_vector(3 downto 3); cyxeguu : inout time);
end sckx;

library ieee;
use ieee.std_logic_1164.all;

architecture vq of sckx is
  signal iytapg : severity_level;
  signal kzqrcv : severity_level;
  signal iqtpfxkfsi : severity_level;
  signal pgrizvtfur : bit_vector(0 downto 3);
  signal uj : std_logic;
begin
  tgxwfozxkv : entity work.xlxtqzdp
    port map (pwgnvxty => uj, we => pgrizvtfur);
  zjco : entity work.lysdmbw
    port map (fnrmtlwx => iqtpfxkfsi);
  bnxcluxr : entity work.lysdmbw
    port map (fnrmtlwx => kzqrcv);
  hrqbt : entity work.lysdmbw
    port map (fnrmtlwx => iytapg);
  
  -- Single-driven assignments
  cyxeguu <= 16#97FB.21# ns;
end vq;



-- Seed after: 7394752682448244163,4404421571376382767
