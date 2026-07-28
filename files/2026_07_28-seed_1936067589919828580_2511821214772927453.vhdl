-- Seed: 1936067589919828580,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity dlnnastcxi is
  port (bulkjydnmn : buffer std_logic; aq : buffer integer; bhd : linkage std_logic; xsn : in time_vector(4 to 2));
end dlnnastcxi;

architecture cdtfd of dlnnastcxi is
  
begin
  -- Single-driven assignments
  aq <= 16#94#;
  
  -- Multi-driven assignments
  bulkjydnmn <= '-';
end cdtfd;

entity vgyvyfabj is
  port (a : linkage real);
end vgyvyfabj;

library ieee;
use ieee.std_logic_1164.all;

architecture d of vgyvyfabj is
  signal ias : std_logic;
  signal sabkhq : integer;
  signal lgikil : std_logic;
  signal hb : time_vector(4 to 2);
  signal euyeug : std_logic;
  signal dqlszc : integer;
  signal qmcxixh : std_logic;
  signal nztlt : time_vector(4 to 2);
  signal ngxpk : integer;
  signal m : std_logic;
begin
  ej : entity work.dlnnastcxi
    port map (bulkjydnmn => m, aq => ngxpk, bhd => m, xsn => nztlt);
  bugaxkt : entity work.dlnnastcxi
    port map (bulkjydnmn => qmcxixh, aq => dqlszc, bhd => euyeug, xsn => hb);
  mnadcsg : entity work.dlnnastcxi
    port map (bulkjydnmn => lgikil, aq => sabkhq, bhd => ias, xsn => hb);
  
  -- Single-driven assignments
  nztlt <= (others => 0 ns);
  hb <= (others => 0 ns);
  
  -- Multi-driven assignments
  qmcxixh <= '1';
end d;



-- Seed after: 5035552129762061220,2511821214772927453
