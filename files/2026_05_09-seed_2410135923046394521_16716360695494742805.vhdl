-- Seed: 2410135923046394521,16716360695494742805



entity xkc is
  port (rtgnc : buffer real);
end xkc;



architecture iettv of xkc is
  
begin
  
end iettv;



entity bkdrhn is
  port (cmyacxtu : in real; mpjt : buffer time; ykzrvjwfz : in integer; hujkzxxq : out real);
end bkdrhn;



architecture xiiooffqb of bkdrhn is
  
begin
  n : entity work.xkc
    port map (rtgnc => hujkzxxq);
end xiiooffqb;



entity vf is
  port (ihkwogrml : in time; as : inout time; cxrtjm : buffer time);
end vf;



architecture oyrjlouzwj of vf is
  signal ohgqsfdww : integer;
  signal tghzruajum : time;
  signal us : real;
begin
  rqrfbdpghz : entity work.bkdrhn
    port map (cmyacxtu => us, mpjt => tghzruajum, ykzrvjwfz => ohgqsfdww, hujkzxxq => us);
end oyrjlouzwj;



-- Seed after: 15378744218322111353,16716360695494742805
