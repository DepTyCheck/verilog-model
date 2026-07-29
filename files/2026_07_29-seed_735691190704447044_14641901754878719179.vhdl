-- Seed: 735691190704447044,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity eld is
  port (sjyjoo : out integer_vector(2 downto 4); rinqsz : out real; tuthlso : in std_logic_vector(1 downto 4));
end eld;

architecture wo of eld is
  
begin
  
end wo;

library ieee;
use ieee.std_logic_1164.all;

entity hvwvzilqc is
  port (lj : linkage std_logic; fjxaw : buffer std_logic);
end hvwvzilqc;

library ieee;
use ieee.std_logic_1164.all;

architecture j of hvwvzilqc is
  signal h : real;
  signal svodrfgmf : integer_vector(2 downto 4);
  signal iijayh : real;
  signal xx : integer_vector(2 downto 4);
  signal fcltyurjct : std_logic_vector(1 downto 4);
  signal cdiehmseu : real;
  signal ke : integer_vector(2 downto 4);
begin
  nnnds : entity work.eld
    port map (sjyjoo => ke, rinqsz => cdiehmseu, tuthlso => fcltyurjct);
  rejcw : entity work.eld
    port map (sjyjoo => xx, rinqsz => iijayh, tuthlso => fcltyurjct);
  mnilcpj : entity work.eld
    port map (sjyjoo => svodrfgmf, rinqsz => h, tuthlso => fcltyurjct);
end j;

library ieee;
use ieee.std_logic_1164.all;

entity xlglavy is
  port (igntlv : out std_logic_vector(1 to 1));
end xlglavy;

library ieee;
use ieee.std_logic_1164.all;

architecture jfxqmeaao of xlglavy is
  signal edjuv : std_logic_vector(1 downto 4);
  signal lfp : real;
  signal w : integer_vector(2 downto 4);
  signal zhngkd : real;
  signal relosiowut : integer_vector(2 downto 4);
  signal zzxlszf : std_logic_vector(1 downto 4);
  signal rsf : real;
  signal j : integer_vector(2 downto 4);
  signal tct : std_logic;
begin
  pwbrtl : entity work.hvwvzilqc
    port map (lj => tct, fjxaw => tct);
  xufshpkv : entity work.eld
    port map (sjyjoo => j, rinqsz => rsf, tuthlso => zzxlszf);
  pqhqkdzhtt : entity work.eld
    port map (sjyjoo => relosiowut, rinqsz => zhngkd, tuthlso => zzxlszf);
  vz : entity work.eld
    port map (sjyjoo => w, rinqsz => lfp, tuthlso => edjuv);
  
  -- Multi-driven assignments
  igntlv <= igntlv;
  igntlv <= "Z";
end jfxqmeaao;

entity vrzzzuv is
  port (ojplns : inout time; fpsoqvha : linkage integer; oahjquls : out boolean);
end vrzzzuv;

architecture adtbcr of vrzzzuv is
  
begin
  -- Single-driven assignments
  ojplns <= 4 sec;
  oahjquls <= FALSE;
end adtbcr;



-- Seed after: 1472905957429314013,14641901754878719179
