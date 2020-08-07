%% Runs on Matlab R2018b
%% Author: Maxime Deforet CC-BY-SA 2020

%% Identify blocks in raw data

str = fileread( '/Users/maxdeforet 1/Downloads/pmc_result.txt');
str=splitlines(str);
blocks_pmc = find(contains( str, 'PMC - PMC' ));
blocks_received = find(contains( str, '[received]' ));
blocks_accepted = find(contains( str, '[accepted]' ));
blocks_doi = find(contains( str, '[doi]' ));
blocks_articletype = find(contains( str, 'PT  - ' ));

T = table('Size',[numel(blocks_pmc),6],'VariableTypes',{'string','string','string','datetime','datetime','double'},'VariableNames',{'PMCID','DOI','ArticleType','Received','Accepted','ReviewTime'});
%%
for i=1:numel(blocks_pmc)-1
    % progress bar:
    if mod(i,100)==0
        i
    end
    counter=0;
    
    % PMC:
    pmc=str{blocks_pmc(i)};
    Tpmc=pmc(7:end);
    
    % DOI:
    ff = find(blocks_doi<blocks_pmc(i+1) & blocks_pmc(i)<blocks_doi);
    if ~isempty(ff)
        doi = str{blocks_doi(ff)};
        Tdoi = doi(7:end-6);
    else
        Tdoi='';
    end
    
    % Article Type:
    ff = find(blocks_articletype<blocks_pmc(i+1) & blocks_pmc(i)<blocks_articletype);
    if ~isempty(ff)
        arttype = str{blocks_articletype(ff(1))};
        arttype = arttype(7:end);
        if length(ff)==2
            arttype2 = str{blocks_articletype(ff(2))};
            arttype = [arttype,' / ',arttype2(7:end)];
        end
        Tarttype = arttype;
    else
        Tarttype='';
    end
    
    % Received:
    ff = find(blocks_received<blocks_pmc(i+1) & blocks_pmc(i)<blocks_received);
    if ~isempty(ff)
        rec = str{blocks_received(ff)};
        if rec(18)=='['
            Trec = datetime(rec(7:16),'InputFormat','yyyy/MM/dd');
            counter=counter+1;
        else
            Trec=NaT;
        end
        
    else
        Trec=NaT;
    end
    
    % Accepted:
    ff = find(blocks_accepted<blocks_pmc(i+1) & blocks_pmc(i)<blocks_accepted);
    if ~isempty(ff)
        acc = str{blocks_accepted(ff)};
        if acc(18)=='['
            Tacc = datetime(acc(7:16),'InputFormat','yyyy/MM/dd');
            counter=counter+1;
        else
            Tacc=NaT;
        end
    else
        Tacc=NaT;
    end
    
    % Review time
    if counter==2
        Tday=days(Tacc-Trec);
    else
        Tday=NaN;
    end
    T(i,:)={Tpmc,Tdoi,Tarttype,Trec,Tacc,Tday};
end


%% Only zero day
T0=T;
T0(isnan(T.ReviewTime) |T.ReviewTime>0 | T.ReviewTime<0 | T.Accepted==NaT,:)=[];
writetable(T0,'ReviewTime_0day.csv')

%% Only one day
T1=T;
T1(isnan(T.ReviewTime) |T.ReviewTime>1 | T.ReviewTime<1 | T.Accepted==NaT,:)=[];
writetable(T1,'ReviewTime_1day.csv')




