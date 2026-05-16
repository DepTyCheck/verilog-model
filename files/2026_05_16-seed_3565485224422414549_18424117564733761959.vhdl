-- Seed: 3565485224422414549,18424117564733761959



entity fwlr is
  port (rtchr : out severity_level; s : out time);
end fwlr;



architecture xpo of fwlr is
  
begin
  
end xpo;



entity qgnrnzm is
  port (ycrssrc : inout integer; bjzcuzacvb : in real; amkgjxqayg : in severity_level; yfxw : buffer real);
end qgnrnzm;



architecture gzul of qgnrnzm is
  signal x : time;
  signal zfte : severity_level;
  signal bqmvqfdnxf : time;
  signal vqbqh : severity_level;
begin
  ltm : entity work.fwlr
    port map (rtchr => vqbqh, s => bqmvqfdnxf);
  tfxltub : entity work.fwlr
    port map (rtchr => zfte, s => x);
end gzul;



entity lggxse is
  port (ikwanazw : out boolean; vlronwcn : buffer integer; ptakekneu : in real; mizclldps : inout severity_level);
end lggxse;



architecture i of lggxse is
  signal oduasyceh : time;
  signal obxehskc : time;
  signal pitk : severity_level;
  signal bbagj : severity_level;
  signal dwo : real;
  signal ij : integer;
begin
  rejydvxm : entity work.qgnrnzm
    port map (ycrssrc => ij, bjzcuzacvb => dwo, amkgjxqayg => bbagj, yfxw => dwo);
  l : entity work.fwlr
    port map (rtchr => pitk, s => obxehskc);
  ecyfn : entity work.fwlr
    port map (rtchr => mizclldps, s => oduasyceh);
end i;



entity ykjihijs is
  port (ytxkfb : in real);
end ykjihijs;



architecture gglh of ykjihijs is
  signal sekpgnags : time;
  signal yg : severity_level;
begin
  lu : entity work.fwlr
    port map (rtchr => yg, s => sekpgnags);
end gglh;



-- Seed after: 13870547116423959619,18424117564733761959
