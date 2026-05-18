-- Seed: 17103272206100028584,5362988889992816031



entity mtdpptk is
  port (u : out integer; ge : in real; xagbkeo : linkage integer);
end mtdpptk;



architecture xcimi of mtdpptk is
  
begin
  
end xcimi;



entity cseqm is
  port (o : in time; ippgctasi : buffer boolean);
end cseqm;



architecture zbk of cseqm is
  signal exw : real;
  signal pzcsh : integer;
  signal ttrwfmh : integer;
  signal kttkmfjr : integer;
  signal lfihnnqur : real;
  signal tag : integer;
begin
  ztpsaffhm : entity work.mtdpptk
    port map (u => tag, ge => lfihnnqur, xagbkeo => kttkmfjr);
  gjty : entity work.mtdpptk
    port map (u => kttkmfjr, ge => lfihnnqur, xagbkeo => tag);
  dzvokwbf : entity work.mtdpptk
    port map (u => ttrwfmh, ge => lfihnnqur, xagbkeo => tag);
  unvucd : entity work.mtdpptk
    port map (u => pzcsh, ge => exw, xagbkeo => tag);
end zbk;



-- Seed after: 14558449983877700878,5362988889992816031
