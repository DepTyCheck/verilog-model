-- Seed: 8604155619969388274,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity idame is
  port (ksfbcc : inout std_logic; yavk : buffer boolean; icrxzjybdo : buffer real);
end idame;

architecture srnreudea of idame is
  
begin
  -- Single-driven assignments
  icrxzjybdo <= 16#451.3#;
  yavk <= TRUE;
  
  -- Multi-driven assignments
  ksfbcc <= ksfbcc;
end srnreudea;

entity wkwylc is
  port (q : out real);
end wkwylc;

architecture vj of wkwylc is
  
begin
  
end vj;

library ieee;
use ieee.std_logic_1164.all;

entity ouybufdl is
  port (th : buffer std_logic; xlf : out real);
end ouybufdl;

architecture mfjm of ouybufdl is
  signal jbqouaiftc : real;
  signal da : boolean;
begin
  bc : entity work.wkwylc
    port map (q => xlf);
  gdn : entity work.idame
    port map (ksfbcc => th, yavk => da, icrxzjybdo => jbqouaiftc);
  
  -- Multi-driven assignments
  th <= 'W';
  th <= 'H';
  th <= th;
  th <= '0';
end mfjm;

entity pwegsz is
  port (plgdrqdd : buffer severity_level);
end pwegsz;

library ieee;
use ieee.std_logic_1164.all;

architecture qwydpj of pwegsz is
  signal svyh : real;
  signal nwpjji : boolean;
  signal gfroayjqze : std_logic;
  signal jca : real;
  signal umtbya : boolean;
  signal zzjq : std_logic;
  signal j : real;
  signal id : boolean;
  signal xtilq : real;
  signal ohxiwudb : boolean;
  signal hq : std_logic;
begin
  dqqpi : entity work.idame
    port map (ksfbcc => hq, yavk => ohxiwudb, icrxzjybdo => xtilq);
  uquhk : entity work.idame
    port map (ksfbcc => hq, yavk => id, icrxzjybdo => j);
  dozpeq : entity work.idame
    port map (ksfbcc => zzjq, yavk => umtbya, icrxzjybdo => jca);
  ssh : entity work.idame
    port map (ksfbcc => gfroayjqze, yavk => nwpjji, icrxzjybdo => svyh);
end qwydpj;



-- Seed after: 11140743187034645275,13613332369802491303
