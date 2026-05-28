-- Seed: 16008742319495150613,6329330932550885447



entity sxuhymnm is
  port (ok : buffer time; ictwcuwyls : in real; fn : inout time_vector(4 to 4));
end sxuhymnm;



architecture pmuhkcej of sxuhymnm is
  
begin
  
end pmuhkcej;

library ieee;
use ieee.std_logic_1164.all;

entity qclvf is
  port (aaji : linkage character; urs : in std_logic; diadileej : linkage std_logic_vector(4 downto 1); mjbevpchlv : in time);
end qclvf;



architecture ulxcz of qclvf is
  signal rjbryucuid : time_vector(4 to 4);
  signal rqssu : real;
  signal kr : time;
begin
  ybzmeifmvh : entity work.sxuhymnm
    port map (ok => kr, ictwcuwyls => rqssu, fn => rjbryucuid);
end ulxcz;



entity omlx is
  port (klyojspj : out bit_vector(4 to 2); skbo : in boolean_vector(2 downto 0); ycqmhg : linkage integer);
end omlx;



architecture jknfm of omlx is
  signal qietauq : time_vector(4 to 4);
  signal ujh : time;
  signal kujkiuy : time_vector(4 to 4);
  signal bflhxqhhfp : real;
  signal rqilyaid : time;
begin
  dbcpyubnum : entity work.sxuhymnm
    port map (ok => rqilyaid, ictwcuwyls => bflhxqhhfp, fn => kujkiuy);
  ytlecqsq : entity work.sxuhymnm
    port map (ok => ujh, ictwcuwyls => bflhxqhhfp, fn => qietauq);
end jknfm;

library ieee;
use ieee.std_logic_1164.all;

entity ba is
  port (b : out std_logic; cz : out std_logic_vector(2 to 0); blawnxsn : buffer time; iuwulktvd : inout integer);
end ba;

library ieee;
use ieee.std_logic_1164.all;

architecture v of ba is
  signal blqbwzviz : time;
  signal phohj : std_logic_vector(4 downto 1);
  signal puq : std_logic;
  signal bhpiypaat : character;
  signal txhtnhc : time_vector(4 to 4);
  signal yugwdasmvj : real;
  signal cgx : time;
  signal zoessauh : time_vector(4 to 4);
  signal zqpqmfkdf : real;
  signal s : boolean_vector(2 downto 0);
  signal ueoyyq : bit_vector(4 to 2);
begin
  gr : entity work.omlx
    port map (klyojspj => ueoyyq, skbo => s, ycqmhg => iuwulktvd);
  cmarlit : entity work.sxuhymnm
    port map (ok => blawnxsn, ictwcuwyls => zqpqmfkdf, fn => zoessauh);
  lufrli : entity work.sxuhymnm
    port map (ok => cgx, ictwcuwyls => yugwdasmvj, fn => txhtnhc);
  xuyi : entity work.qclvf
    port map (aaji => bhpiypaat, urs => puq, diadileej => phohj, mjbevpchlv => blqbwzviz);
end v;



-- Seed after: 15078323621620276592,6329330932550885447
