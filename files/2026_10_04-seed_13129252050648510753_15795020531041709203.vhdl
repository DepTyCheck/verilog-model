-- Seed: 13129252050648510753,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity udkk is
  port (lrd : out boolean_vector(4 to 2); n : inout boolean_vector(1 to 1); ael : inout std_logic; fdmbklosl : inout std_logic);
end udkk;

architecture bfm of udkk is
  
begin
  -- Single-driven assignments
  n <= n;
  lrd <= lrd;
  
  -- Multi-driven assignments
  ael <= 'X';
  ael <= fdmbklosl;
  ael <= ael;
  ael <= 'L';
end bfm;

entity rllphazxn is
  port (gfz : inout bit_vector(4 to 3); huvxi : out bit; g : linkage character; nwwsoxwxkl : out bit_vector(1 to 0));
end rllphazxn;

library ieee;
use ieee.std_logic_1164.all;

architecture kcrhzymsy of rllphazxn is
  signal qrepyohesd : boolean_vector(1 to 1);
  signal hux : boolean_vector(4 to 2);
  signal jphfr : std_logic;
  signal bsan : boolean_vector(1 to 1);
  signal apnnmtzv : boolean_vector(4 to 2);
  signal fznp : std_logic;
  signal lqymtvvcft : boolean_vector(1 to 1);
  signal blq : boolean_vector(4 to 2);
begin
  q : entity work.udkk
    port map (lrd => blq, n => lqymtvvcft, ael => fznp, fdmbklosl => fznp);
  upzjptvy : entity work.udkk
    port map (lrd => apnnmtzv, n => bsan, ael => fznp, fdmbklosl => jphfr);
  lnynjyxxm : entity work.udkk
    port map (lrd => hux, n => qrepyohesd, ael => jphfr, fdmbklosl => fznp);
  
  -- Single-driven assignments
  nwwsoxwxkl <= nwwsoxwxkl;
  gfz <= (others => '0');
  huvxi <= '0';
  
  -- Multi-driven assignments
  jphfr <= jphfr;
  fznp <= '-';
end kcrhzymsy;

entity cs is
  port (vvkl : inout bit_vector(4 to 2));
end cs;

library ieee;
use ieee.std_logic_1164.all;

architecture p of cs is
  signal sgbhteqom : std_logic;
  signal gctleg : boolean_vector(1 to 1);
  signal qrchxwcib : boolean_vector(4 to 2);
  signal zpmmu : std_logic;
  signal zrysuswqwt : std_logic;
  signal o : boolean_vector(1 to 1);
  signal jn : boolean_vector(4 to 2);
  signal gdmx : std_logic;
  signal vqffry : boolean_vector(1 to 1);
  signal ytovi : boolean_vector(4 to 2);
  signal emueatvc : std_logic;
  signal aspepvedpk : std_logic;
  signal mvt : boolean_vector(1 to 1);
  signal iykd : boolean_vector(4 to 2);
begin
  zfyqpav : entity work.udkk
    port map (lrd => iykd, n => mvt, ael => aspepvedpk, fdmbklosl => emueatvc);
  lhtlzxb : entity work.udkk
    port map (lrd => ytovi, n => vqffry, ael => gdmx, fdmbklosl => aspepvedpk);
  lq : entity work.udkk
    port map (lrd => jn, n => o, ael => zrysuswqwt, fdmbklosl => zpmmu);
  h : entity work.udkk
    port map (lrd => qrchxwcib, n => gctleg, ael => aspepvedpk, fdmbklosl => sgbhteqom);
  
  -- Single-driven assignments
  vvkl <= vvkl;
end p;



-- Seed after: 12972784038971934022,15795020531041709203
