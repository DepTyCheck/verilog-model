-- Seed: 16161635544734280651,4860866131898729603

entity abclhkhqw is
  port (wnknigdx : buffer character; xpflqrxb : in severity_level; ykeoitrzug : buffer time);
end abclhkhqw;

architecture nlujf of abclhkhqw is
  
begin
  -- Single-driven assignments
  ykeoitrzug <= 0 sec;
  wnknigdx <= 'o';
end nlujf;

entity pnrtjumnbc is
  port (jkpavcwkyo : buffer time; egzinir : in integer; vyelollqpv : in time; hx : inout time);
end pnrtjumnbc;

architecture fjgms of pnrtjumnbc is
  signal sccquxi : severity_level;
  signal f : character;
  signal tp : time;
  signal vnzxada : severity_level;
  signal rf : character;
  signal mwzupje : severity_level;
  signal qlch : character;
begin
  ebt : entity work.abclhkhqw
    port map (wnknigdx => qlch, xpflqrxb => mwzupje, ykeoitrzug => hx);
  oy : entity work.abclhkhqw
    port map (wnknigdx => rf, xpflqrxb => vnzxada, ykeoitrzug => tp);
  uenf : entity work.abclhkhqw
    port map (wnknigdx => f, xpflqrxb => sccquxi, ykeoitrzug => jkpavcwkyo);
  
  -- Single-driven assignments
  vnzxada <= ERROR;
  sccquxi <= ERROR;
  mwzupje <= WARNING;
end fjgms;

entity mov is
  port (iggbj : inout character; xn : inout real; bxjqxwevsf : buffer time);
end mov;

architecture biyajtpfod of mov is
  signal exix : severity_level;
  signal mdzzkmsm : severity_level;
  signal xahkeem : character;
  signal wjvnah : time;
  signal qgsbvs : time;
  signal wxzy : integer;
  signal gum : time;
begin
  mzvx : entity work.pnrtjumnbc
    port map (jkpavcwkyo => gum, egzinir => wxzy, vyelollqpv => qgsbvs, hx => wjvnah);
  wnii : entity work.abclhkhqw
    port map (wnknigdx => xahkeem, xpflqrxb => mdzzkmsm, ykeoitrzug => qgsbvs);
  gwuskzzkpt : entity work.abclhkhqw
    port map (wnknigdx => iggbj, xpflqrxb => exix, ykeoitrzug => bxjqxwevsf);
end biyajtpfod;



-- Seed after: 10176161963564117137,4860866131898729603
