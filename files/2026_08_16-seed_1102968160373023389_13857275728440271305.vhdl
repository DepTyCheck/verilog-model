-- Seed: 1102968160373023389,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity vgqyrnon is
  port (lpmliztnr : buffer integer_vector(2 to 2); cjcmcuvfch : buffer integer; hro : in std_logic);
end vgqyrnon;

architecture ibfrh of vgqyrnon is
  
begin
  -- Single-driven assignments
  cjcmcuvfch <= 1224;
  lpmliztnr <= (others => 0);
end ibfrh;

entity bvv is
  port (ylwp : inout real; uic : inout time; anlvhwzwv : out integer);
end bvv;

library ieee;
use ieee.std_logic_1164.all;

architecture ljvet of bvv is
  signal lrabmuv : integer_vector(2 to 2);
  signal obdumis : integer;
  signal izeqwhhvh : integer_vector(2 to 2);
  signal bwvuwz : integer;
  signal kh : integer_vector(2 to 2);
  signal dbpe : std_logic;
  signal zg : integer;
  signal jfqsppnxf : integer_vector(2 to 2);
begin
  zpbql : entity work.vgqyrnon
    port map (lpmliztnr => jfqsppnxf, cjcmcuvfch => zg, hro => dbpe);
  wjzmvihb : entity work.vgqyrnon
    port map (lpmliztnr => kh, cjcmcuvfch => bwvuwz, hro => dbpe);
  wn : entity work.vgqyrnon
    port map (lpmliztnr => izeqwhhvh, cjcmcuvfch => obdumis, hro => dbpe);
  anyx : entity work.vgqyrnon
    port map (lpmliztnr => lrabmuv, cjcmcuvfch => anlvhwzwv, hro => dbpe);
  
  -- Single-driven assignments
  uic <= uic;
  ylwp <= ylwp;
  
  -- Multi-driven assignments
  dbpe <= dbpe;
  dbpe <= 'X';
  dbpe <= 'L';
  dbpe <= '1';
end ljvet;



-- Seed after: 7275313824801154021,13857275728440271305
