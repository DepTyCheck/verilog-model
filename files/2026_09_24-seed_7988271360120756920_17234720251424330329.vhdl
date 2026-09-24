-- Seed: 7988271360120756920,17234720251424330329

entity sznb is
  port (qbsgv : in character; kjvcgjfdd : buffer time; dnbtipj : in integer);
end sznb;

architecture piodprwtrp of sznb is
  
begin
  
end piodprwtrp;

entity kuwojq is
  port (nbtxkynhq : in string(2 to 5));
end kuwojq;

architecture pfgaeswsc of kuwojq is
  signal qbazbm : time;
  signal ta : character;
  signal ymjyz : integer;
  signal mwfnk : time;
  signal b : character;
begin
  slaspe : entity work.sznb
    port map (qbsgv => b, kjvcgjfdd => mwfnk, dnbtipj => ymjyz);
  nezmhzntnc : entity work.sznb
    port map (qbsgv => ta, kjvcgjfdd => qbazbm, dnbtipj => ymjyz);
  
  -- Single-driven assignments
  ta <= b;
end pfgaeswsc;

entity gfsm is
  port (fmpexf : inout integer);
end gfsm;

architecture lekvxtni of gfsm is
  
begin
  -- Single-driven assignments
  fmpexf <= fmpexf;
end lekvxtni;

entity iqyplrwfnd is
  port (iz : buffer integer);
end iqyplrwfnd;

architecture x of iqyplrwfnd is
  signal xmxer : string(2 to 5);
  signal tumx : integer;
  signal chjbflqr : integer;
  signal dgkqxfzfs : time;
  signal m : character;
begin
  mrxj : entity work.sznb
    port map (qbsgv => m, kjvcgjfdd => dgkqxfzfs, dnbtipj => chjbflqr);
  d : entity work.gfsm
    port map (fmpexf => tumx);
  jsidkbf : entity work.kuwojq
    port map (nbtxkynhq => xmxer);
  
  -- Single-driven assignments
  iz <= tumx;
  xmxer <= "acod";
  m <= m;
  chjbflqr <= chjbflqr;
end x;



-- Seed after: 13825080008069374231,17234720251424330329
