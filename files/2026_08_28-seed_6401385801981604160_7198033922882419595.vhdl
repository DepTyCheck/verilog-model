-- Seed: 6401385801981604160,7198033922882419595

entity tzlypj is
  port (ebfzydyumn : inout time; jw : out real_vector(4 to 0));
end tzlypj;

architecture udnuu of tzlypj is
  
begin
  -- Single-driven assignments
  ebfzydyumn <= 16#D_0_2_8.D_C# ms;
end udnuu;

entity gafjdh is
  port (nrdfn : buffer time; o : buffer real);
end gafjdh;

architecture qtus of gafjdh is
  signal pqli : real_vector(4 to 0);
  signal t : time;
  signal bggprhhn : real_vector(4 to 0);
  signal wy : real_vector(4 to 0);
  signal yxga : time;
begin
  kiajwem : entity work.tzlypj
    port map (ebfzydyumn => yxga, jw => wy);
  ejhziulgnq : entity work.tzlypj
    port map (ebfzydyumn => nrdfn, jw => bggprhhn);
  iuuh : entity work.tzlypj
    port map (ebfzydyumn => t, jw => pqli);
end qtus;

entity ya is
  port (mtbccs : linkage integer);
end ya;

architecture lwfc of ya is
  signal g : real;
  signal bfkejkhjv : time;
begin
  byungoh : entity work.gafjdh
    port map (nrdfn => bfkejkhjv, o => g);
end lwfc;

entity letbq is
  port (xat : out real);
end letbq;

architecture fhw of letbq is
  signal i : real_vector(4 to 0);
  signal j : time;
  signal k : real_vector(4 to 0);
  signal y : time;
begin
  bdsypjuad : entity work.tzlypj
    port map (ebfzydyumn => y, jw => k);
  ytdgnjiutq : entity work.tzlypj
    port map (ebfzydyumn => j, jw => i);
  
  -- Single-driven assignments
  xat <= xat;
end fhw;



-- Seed after: 10986333908966552814,7198033922882419595
