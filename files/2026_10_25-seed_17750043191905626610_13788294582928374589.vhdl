-- Seed: 17750043191905626610,13788294582928374589

entity brjabajhi is
  port (xcbrz : in time; m : in boolean_vector(3 to 4); vny : buffer severity_level);
end brjabajhi;

architecture lvgxaiom of brjabajhi is
  
begin
  
end lvgxaiom;

entity mn is
  port (ldnysdsu : out severity_level);
end mn;

architecture jftdphpz of mn is
  signal y : severity_level;
  signal chrmw : time;
  signal cinmt : severity_level;
  signal m : boolean_vector(3 to 4);
  signal zwdlqu : severity_level;
  signal lmj : boolean_vector(3 to 4);
  signal hwnp : time;
begin
  lgjih : entity work.brjabajhi
    port map (xcbrz => hwnp, m => lmj, vny => zwdlqu);
  vbtguawd : entity work.brjabajhi
    port map (xcbrz => hwnp, m => m, vny => cinmt);
  aoyjp : entity work.brjabajhi
    port map (xcbrz => chrmw, m => lmj, vny => ldnysdsu);
  jkdvv : entity work.brjabajhi
    port map (xcbrz => hwnp, m => lmj, vny => y);
  
  -- Single-driven assignments
  hwnp <= 1 hr;
  lmj <= lmj;
end jftdphpz;



-- Seed after: 4145312782948153015,13788294582928374589
