-- Seed: 13382178923383887106,16715549879197889543



entity xcdeeto is
  port (duc : linkage real; cojkplkfja : buffer time);
end xcdeeto;



architecture t of xcdeeto is
  
begin
  
end t;



entity w is
  port (bt : in time; zzzmvbqvo : inout real);
end w;



architecture m of w is
  signal cjhpytccxw : time;
  signal rb : time;
  signal zypdywnon : real;
  signal vpummhks : time;
  signal jn : time;
begin
  bxspeays : entity work.xcdeeto
    port map (duc => zzzmvbqvo, cojkplkfja => jn);
  pqdyibzb : entity work.xcdeeto
    port map (duc => zzzmvbqvo, cojkplkfja => vpummhks);
  jq : entity work.xcdeeto
    port map (duc => zypdywnon, cojkplkfja => rb);
  ar : entity work.xcdeeto
    port map (duc => zzzmvbqvo, cojkplkfja => cjhpytccxw);
end m;



entity szjcjlcsb is
  port (azig : out real);
end szjcjlcsb;



architecture v of szjcjlcsb is
  signal tg : time;
  signal ys : time;
begin
  dorxtb : entity work.w
    port map (bt => ys, zzzmvbqvo => azig);
  bkvmlh : entity work.xcdeeto
    port map (duc => azig, cojkplkfja => ys);
  tacmkifkmf : entity work.xcdeeto
    port map (duc => azig, cojkplkfja => tg);
end v;



-- Seed after: 4337147328536379670,16715549879197889543
