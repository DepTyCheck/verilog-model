-- Seed: 4160114862608618471,3042374792655995433

entity isr is
  port (akdq : buffer time);
end isr;

architecture qtl of isr is
  
begin
  -- Single-driven assignments
  akdq <= 2 sec;
end qtl;

entity bwyzzbpgu is
  port (yhue : in time; gziiibm : inout time_vector(4 to 0));
end bwyzzbpgu;

architecture bkse of bwyzzbpgu is
  signal knysmst : time;
begin
  fsq : entity work.isr
    port map (akdq => knysmst);
  
  -- Single-driven assignments
  gziiibm <= (others => 0 ns);
end bkse;

entity pikw is
  port (hvxz : linkage boolean);
end pikw;

architecture nrskufqdbv of pikw is
  signal pivjtmgck : time;
  signal upjezzizf : time_vector(4 to 0);
  signal hytn : time;
  signal hfivizlie : time;
  signal abamns : time;
begin
  uqsxtrao : entity work.isr
    port map (akdq => abamns);
  gfmvh : entity work.isr
    port map (akdq => hfivizlie);
  oy : entity work.bwyzzbpgu
    port map (yhue => hytn, gziiibm => upjezzizf);
  lnbnl : entity work.isr
    port map (akdq => pivjtmgck);
  
  -- Single-driven assignments
  hytn <= 1 hr;
end nrskufqdbv;



-- Seed after: 17992115750536117487,3042374792655995433
