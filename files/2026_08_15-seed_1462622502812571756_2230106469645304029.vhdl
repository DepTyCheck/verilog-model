-- Seed: 1462622502812571756,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity syf is
  port (zgsghagiec : out real; kxcply : buffer std_logic_vector(1 downto 4); qpmkqsum : out time; ncsn : inout boolean);
end syf;

architecture mmkrqvqsjd of syf is
  
begin
  -- Single-driven assignments
  ncsn <= TRUE;
  
  -- Multi-driven assignments
  kxcply <= kxcply;
end mmkrqvqsjd;

library ieee;
use ieee.std_logic_1164.all;

entity fbdzw is
  port (rtunxxj : inout std_logic_vector(2 to 3); tzdeks : in std_logic_vector(3 to 4); wjc : linkage real_vector(2 downto 0); kflel : inout real);
end fbdzw;

library ieee;
use ieee.std_logic_1164.all;

architecture kcgo of fbdzw is
  signal gitjwqu : boolean;
  signal lebgxe : time;
  signal zaz : boolean;
  signal jt : time;
  signal shyysbadxi : real;
  signal efvjvykii : boolean;
  signal sawbhnledo : time;
  signal wpezz : std_logic_vector(1 downto 4);
  signal yyxmdzh : real;
begin
  dtrytimkar : entity work.syf
    port map (zgsghagiec => yyxmdzh, kxcply => wpezz, qpmkqsum => sawbhnledo, ncsn => efvjvykii);
  jvvmpmg : entity work.syf
    port map (zgsghagiec => shyysbadxi, kxcply => wpezz, qpmkqsum => jt, ncsn => zaz);
  ivutolmlee : entity work.syf
    port map (zgsghagiec => kflel, kxcply => wpezz, qpmkqsum => lebgxe, ncsn => gitjwqu);
  
  -- Multi-driven assignments
  rtunxxj <= tzdeks;
  rtunxxj <= tzdeks;
end kcgo;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (kliiryh : linkage bit; rdw : inout std_logic);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture dtowrb of u is
  signal ab : boolean;
  signal qpgaqttj : time;
  signal zmm : real;
  signal lhyy : boolean;
  signal jnlkvfgd : time;
  signal vwf : std_logic_vector(1 downto 4);
  signal azlqu : real;
  signal ismiwsi : real;
  signal hppvv : real_vector(2 downto 0);
  signal cuchqyfn : std_logic_vector(3 to 4);
  signal ueayrv : std_logic_vector(2 to 3);
begin
  cczgmzjk : entity work.fbdzw
    port map (rtunxxj => ueayrv, tzdeks => cuchqyfn, wjc => hppvv, kflel => ismiwsi);
  bydtlwxei : entity work.syf
    port map (zgsghagiec => azlqu, kxcply => vwf, qpmkqsum => jnlkvfgd, ncsn => lhyy);
  ed : entity work.syf
    port map (zgsghagiec => zmm, kxcply => vwf, qpmkqsum => qpgaqttj, ncsn => ab);
  
  -- Multi-driven assignments
  vwf <= (others => '0');
end dtowrb;



-- Seed after: 8590179149653443032,2230106469645304029
