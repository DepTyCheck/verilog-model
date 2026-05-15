-- Seed: 16500630591985620822,1420300365443511127



entity ogfbfstbl is
  port (vxwetxj : out time; khxxomj : linkage time; yarhvp : linkage real);
end ogfbfstbl;



architecture g of ogfbfstbl is
  
begin
  
end g;



entity kcrm is
  port (rsbhmltjxs : in real; n : in integer);
end kcrm;



architecture duvwvq of kcrm is
  signal snyqorpcdr : time;
  signal fry : time;
begin
  wmuyco : entity work.ogfbfstbl
    port map (vxwetxj => fry, khxxomj => snyqorpcdr, yarhvp => rsbhmltjxs);
end duvwvq;



entity biihoj is
  port (rxtxpjmk : inout boolean; kj : inout time; quwset : linkage integer; xpvju : buffer real);
end biihoj;



architecture zyytjr of biihoj is
  signal afipmld : integer;
begin
  flyekodfq : entity work.ogfbfstbl
    port map (vxwetxj => kj, khxxomj => kj, yarhvp => xpvju);
  sqxebnq : entity work.kcrm
    port map (rsbhmltjxs => xpvju, n => afipmld);
end zyytjr;



-- Seed after: 12846390140628265376,1420300365443511127
