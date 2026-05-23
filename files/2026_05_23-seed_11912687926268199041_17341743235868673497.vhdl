-- Seed: 11912687926268199041,17341743235868673497



entity qcmsloyuq is
  port (n : inout real; e : linkage time; wbupdepk : inout time; yhgtdaexn : linkage integer);
end qcmsloyuq;



architecture dmjxnttjqb of qcmsloyuq is
  
begin
  
end dmjxnttjqb;



entity ztkqhjc is
  port (evp : inout time; isg : inout real);
end ztkqhjc;



architecture otlu of ztkqhjc is
  signal fireirg : integer;
  signal mkxgmpb : time;
  signal uxbj : real;
begin
  u : entity work.qcmsloyuq
    port map (n => uxbj, e => evp, wbupdepk => mkxgmpb, yhgtdaexn => fireirg);
  d : entity work.qcmsloyuq
    port map (n => isg, e => evp, wbupdepk => evp, yhgtdaexn => fireirg);
end otlu;



entity sgnylgx is
  port (iotpxrwmvg : buffer time; aohvu : inout bit; kt : out time);
end sgnylgx;



architecture cx of sgnylgx is
  signal ko : real;
  signal ddzrpbjvzr : time;
  signal ntpxtyqbxr : integer;
  signal vybfejlik : time;
  signal ro : time;
  signal ru : real;
begin
  nromcxrjth : entity work.qcmsloyuq
    port map (n => ru, e => ro, wbupdepk => vybfejlik, yhgtdaexn => ntpxtyqbxr);
  rcpoyfpf : entity work.ztkqhjc
    port map (evp => ddzrpbjvzr, isg => ko);
end cx;



-- Seed after: 1897046556455628203,17341743235868673497
