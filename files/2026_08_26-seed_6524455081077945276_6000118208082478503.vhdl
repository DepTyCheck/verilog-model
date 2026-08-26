-- Seed: 6524455081077945276,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity zipnvmthj is
  port (mrejtquye : inout std_logic; hv : inout std_logic_vector(3 downto 0); mfwoml : out bit);
end zipnvmthj;

architecture skqzo of zipnvmthj is
  
begin
  
end skqzo;

library ieee;
use ieee.std_logic_1164.all;

entity jxcde is
  port (wvz : buffer std_logic; gcn : buffer std_logic);
end jxcde;

library ieee;
use ieee.std_logic_1164.all;

architecture myeigp of jxcde is
  signal oxv : bit;
  signal tajhxoyo : bit;
  signal dbx : std_logic_vector(3 downto 0);
  signal mubcgvkxsi : bit;
  signal ckl : std_logic_vector(3 downto 0);
begin
  fndvr : entity work.zipnvmthj
    port map (mrejtquye => gcn, hv => ckl, mfwoml => mubcgvkxsi);
  ojxikelan : entity work.zipnvmthj
    port map (mrejtquye => wvz, hv => dbx, mfwoml => tajhxoyo);
  nmfsdy : entity work.zipnvmthj
    port map (mrejtquye => gcn, hv => ckl, mfwoml => oxv);
  
  -- Multi-driven assignments
  gcn <= 'L';
end myeigp;

entity g is
  port (cof : inout severity_level);
end g;

architecture zpvv of g is
  
begin
  -- Single-driven assignments
  cof <= WARNING;
end zpvv;



-- Seed after: 11997807925099318935,6000118208082478503
