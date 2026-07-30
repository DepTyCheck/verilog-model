-- Seed: 10974083911714747113,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity yhtcaysp is
  port ( fhigabwxru : linkage std_logic_vector(0 downto 2)
  ; wweyjgqgr : linkage std_logic_vector(2 downto 3)
  ; m : buffer integer
  ; fpxxfimsn : out boolean_vector(2 to 1)
  );
end yhtcaysp;

architecture lvypvalega of yhtcaysp is
  
begin
  -- Single-driven assignments
  fpxxfimsn <= (others => TRUE);
  m <= 16#7B#;
end lvypvalega;

entity kblmmbfc is
  port (vihdn : inout real; iuwc : in integer; mtech : buffer severity_level; as : buffer character);
end kblmmbfc;

library ieee;
use ieee.std_logic_1164.all;

architecture vbliwihsn of kblmmbfc is
  signal bhsgz : boolean_vector(2 to 1);
  signal k : integer;
  signal bouui : std_logic_vector(0 downto 2);
  signal u : boolean_vector(2 to 1);
  signal lqr : integer;
  signal brfrftrq : std_logic_vector(2 downto 3);
  signal obzu : std_logic_vector(2 downto 3);
  signal ksilepxd : boolean_vector(2 to 1);
  signal hqhstvdxm : integer;
  signal kqdqaiv : std_logic_vector(2 downto 3);
begin
  jhkl : entity work.yhtcaysp
    port map (fhigabwxru => kqdqaiv, wweyjgqgr => kqdqaiv, m => hqhstvdxm, fpxxfimsn => ksilepxd);
  qrwa : entity work.yhtcaysp
    port map (fhigabwxru => obzu, wweyjgqgr => brfrftrq, m => lqr, fpxxfimsn => u);
  ciftbmsj : entity work.yhtcaysp
    port map (fhigabwxru => bouui, wweyjgqgr => obzu, m => k, fpxxfimsn => bhsgz);
  
  -- Single-driven assignments
  as <= 'z';
  vihdn <= vihdn;
  mtech <= NOTE;
  
  -- Multi-driven assignments
  brfrftrq <= kqdqaiv;
  bouui <= "";
end vbliwihsn;

library ieee;
use ieee.std_logic_1164.all;

entity ndcsyo is
  port (g : in time; fegyqutwqr : inout std_logic_vector(0 to 4); otqv : linkage time);
end ndcsyo;

library ieee;
use ieee.std_logic_1164.all;

architecture ha of ndcsyo is
  signal audatocl : boolean_vector(2 to 1);
  signal krjdsn : integer;
  signal nyajhevfl : std_logic_vector(2 downto 3);
  signal szcify : boolean_vector(2 to 1);
  signal klohdfp : std_logic_vector(2 downto 3);
  signal ujfzxbj : std_logic_vector(0 downto 2);
  signal qongpyoep : character;
  signal jzqqf : severity_level;
  signal qbgujkrv : integer;
  signal yopuutx : real;
begin
  zcwbschx : entity work.kblmmbfc
    port map (vihdn => yopuutx, iuwc => qbgujkrv, mtech => jzqqf, as => qongpyoep);
  bqshr : entity work.yhtcaysp
    port map (fhigabwxru => ujfzxbj, wweyjgqgr => klohdfp, m => qbgujkrv, fpxxfimsn => szcify);
  nbpfj : entity work.yhtcaysp
    port map (fhigabwxru => nyajhevfl, wweyjgqgr => nyajhevfl, m => krjdsn, fpxxfimsn => audatocl);
end ha;



-- Seed after: 2731656042489764135,4122021602305298647
