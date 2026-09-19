-- Seed: 4116041193952488540,14141408946471626091

entity iwvvy is
  port (wtepjygqtr : in integer; crfiiqzv : out time_vector(3 downto 0); vcza : buffer integer; mis : in time);
end iwvvy;

architecture tksfkkcgu of iwvvy is
  
begin
  -- Single-driven assignments
  vcza <= 16#51#;
  crfiiqzv <= (2#0.0_0# ns, 2#0010# ps, 1 hr, 8#51# ms);
end tksfkkcgu;

entity yts is
  port (li : linkage time_vector(4 to 4); oiaye : buffer severity_level);
end yts;

architecture vgtmmo of yts is
  signal h : time;
  signal tzax : integer;
  signal jdqla : time_vector(3 downto 0);
  signal ncs : integer;
  signal qbonh : time;
  signal reu : integer;
  signal ov : time_vector(3 downto 0);
  signal kbw : time;
  signal iuqge : time_vector(3 downto 0);
  signal sarhko : integer;
begin
  eupj : entity work.iwvvy
    port map (wtepjygqtr => sarhko, crfiiqzv => iuqge, vcza => sarhko, mis => kbw);
  auwh : entity work.iwvvy
    port map (wtepjygqtr => sarhko, crfiiqzv => ov, vcza => reu, mis => qbonh);
  gpcukasib : entity work.iwvvy
    port map (wtepjygqtr => ncs, crfiiqzv => jdqla, vcza => tzax, mis => h);
  
  -- Single-driven assignments
  oiaye <= ERROR;
  kbw <= 1 min;
end vgtmmo;

library ieee;
use ieee.std_logic_1164.all;

entity eevbbwdtdw is
  port (b : linkage std_logic; otgvjtwgrx : in integer);
end eevbbwdtdw;

architecture ig of eevbbwdtdw is
  signal hsx : severity_level;
  signal yoeerdgr : time_vector(4 to 4);
  signal mlfiydfos : severity_level;
  signal eicbd : time_vector(4 to 4);
begin
  udaaexzwi : entity work.yts
    port map (li => eicbd, oiaye => mlfiydfos);
  pxyhotnfn : entity work.yts
    port map (li => yoeerdgr, oiaye => hsx);
end ig;



-- Seed after: 6069242732169123986,14141408946471626091
