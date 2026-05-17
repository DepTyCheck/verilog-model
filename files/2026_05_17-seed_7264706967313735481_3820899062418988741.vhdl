-- Seed: 7264706967313735481,3820899062418988741



entity ciudgiabj is
  port (fiaemjeof : inout real; pcvypnyvxb : linkage boolean; acdzzkjcbv : out boolean);
end ciudgiabj;



architecture wvm of ciudgiabj is
  
begin
  
end wvm;



entity a is
  port (egbbpmu : inout boolean; mmi : buffer time; kjkn : linkage time);
end a;



architecture yftfvxbgkt of a is
  signal dexspfazd : real;
  signal wsdubhuzmp : boolean;
  signal iyioe : boolean;
  signal g : real;
begin
  azj : entity work.ciudgiabj
    port map (fiaemjeof => g, pcvypnyvxb => iyioe, acdzzkjcbv => wsdubhuzmp);
  fburc : entity work.ciudgiabj
    port map (fiaemjeof => dexspfazd, pcvypnyvxb => egbbpmu, acdzzkjcbv => egbbpmu);
end yftfvxbgkt;



entity uz is
  port (lbatfwg : linkage time; u : linkage integer; oifpzy : buffer real);
end uz;



architecture rloaozsqmr of uz is
  signal k : time;
  signal o : time;
  signal uia : boolean;
  signal xjrduunri : boolean;
  signal lmtrfzuus : time;
  signal mkoczmep : time;
  signal kfqb : boolean;
begin
  nyybshw : entity work.a
    port map (egbbpmu => kfqb, mmi => mkoczmep, kjkn => lmtrfzuus);
  ubsyrcg : entity work.ciudgiabj
    port map (fiaemjeof => oifpzy, pcvypnyvxb => xjrduunri, acdzzkjcbv => xjrduunri);
  r : entity work.a
    port map (egbbpmu => uia, mmi => o, kjkn => k);
end rloaozsqmr;



-- Seed after: 11087124181867219647,3820899062418988741
