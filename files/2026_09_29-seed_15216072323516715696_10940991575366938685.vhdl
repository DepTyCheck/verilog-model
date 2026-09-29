-- Seed: 15216072323516715696,10940991575366938685

entity uz is
  port (oua : in real; tcfycre : inout real);
end uz;

architecture t of uz is
  
begin
  -- Single-driven assignments
  tcfycre <= 0.4;
end t;

entity mkj is
  port (ocqscdyb : out time; mtxfmx : out time);
end mkj;

architecture fgrxo of mkj is
  signal djtbrm : real;
  signal axpnswp : real;
  signal vp : real;
  signal qe : real;
begin
  yibp : entity work.uz
    port map (oua => qe, tcfycre => vp);
  gmgsxgtrvq : entity work.uz
    port map (oua => axpnswp, tcfycre => qe);
  sv : entity work.uz
    port map (oua => qe, tcfycre => djtbrm);
  ifcokvbae : entity work.uz
    port map (oua => djtbrm, tcfycre => axpnswp);
  
  -- Single-driven assignments
  mtxfmx <= 1 hr;
  ocqscdyb <= 8#6# ps;
end fgrxo;

entity j is
  port (spyvdn : linkage character);
end j;

architecture kibliomce of j is
  signal vysolpnw : real;
  signal fo : real;
  signal nzojlyopg : real;
  signal kurd : real;
begin
  njwqfyq : entity work.uz
    port map (oua => kurd, tcfycre => nzojlyopg);
  oxsfpxkhmy : entity work.uz
    port map (oua => kurd, tcfycre => kurd);
  e : entity work.uz
    port map (oua => fo, tcfycre => vysolpnw);
  hethijus : entity work.uz
    port map (oua => kurd, tcfycre => fo);
end kibliomce;



-- Seed after: 5202816226351920622,10940991575366938685
